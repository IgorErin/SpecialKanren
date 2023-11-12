open Value
open Dnf

type ('a, 'b) result =
  { globals : Ident.t list
  ; dnf : ('a, 'b) dnf option
  }

module Propagate = struct
  let travers_graph cnj =
    let info =
      List.map
        (function
         | DUnify (ident, Var v) -> [ ident, Var v; v, Var ident ]
         | DUnify (ident, (Constr _ as c)) -> [ ident, c ]
         | _ -> [])
        cnj
      |> List.concat
    in
    let names = List.map fst info in
    (* bfs *)
    let get_values name =
      let isame = Ident.same in
      let vsame = Value.same in
      let all_values_by_ident soruce_name =
        List.filter_map
          (fun (name, value) -> if isame soruce_name name then Some value else None)
          info
      in
      let all_valeus_by_value = function
        | Var ident as v ->
          (* todo strange add *)
          let next = all_values_by_ident ident in
          [ v ] @ next
        | Constr _ as c -> Core.List.return c
      in
      let rec loop acc front =
        let new_front = List.map all_valeus_by_value front |> List.concat in
        let diff =
          List.filter (fun name -> not @@ List.exists (vsame name) acc) new_front
        in
        match diff with
        | [] -> acc
        | _ :: _ -> loop (acc @ diff) new_front
      in
      let result = loop [] [ Var name ] in
      name, result
    in
    names |> List.map get_values
  ;;

  let infer_result values =
    let vars, consts = Core.List.partition_map ~f:Value.partition values in
    match vars, consts with
    | _, [] -> None
    | _, ls ->
      let is_ground x = Value.is_ground @@ Constr x in
      List.filter is_ground ls
      |> (function
      | [] -> None
      | hd :: _ -> Some hd)
  ;;

  let try_propagate conj =
    let values = travers_graph conj in
    let map =
      List.filter_map
        (fun (name, value) -> Option.map (fun vl -> name, vl) @@ infer_result value)
        values
    in
    let subst_var = function
      | Var v ->
        let same = Ident.same v in
        List.find_opt (fun x -> x |> fst |> same) map
        |> (function
        | Some pair ->
          let const = snd pair in
          Constr const
        | _ -> Var v)
      | x -> x
    in
    let map_call = function
      | DCall (name, values) ->
        let new_values = List.map subst_var values in
        DCall (name, new_values)
      | x -> x
    in
    List.map map_call conj
  ;;
end

exception Reduced

let reduce_const_const (dnf : _ cnj) : _ cnj option =
  let lr = Core.List.return in
  let reduced () = raise Reduced in
  let rec loop = function
    | DUnify (Var v, next) | DUnify (next, Var v) -> DUnify (v, next) |> lr
    | DDisunify (Var v, next) | DDisunify (next, Var v) -> DDisunify (v, next) |> lr
    | DCall (ident, values) -> DCall (ident, values) |> lr
    | DFresh _ as f -> f |> lr
    | DUnify (Constr (ld, lv), Constr (rd, rv)) ->
      if Types.may_equal_constr ld rd
      then List.map2 (fun lv rv -> loop @@ DUnify (lv, rv)) lv rv |> List.concat
      else reduced ()
    | DDisunify (Constr (ld, lv), Constr (rd, rv)) ->
      if Types.may_equal_constr ld rd
      then List.map2 (fun lv rv -> loop @@ DDisunify (lv, rv)) lv rv |> List.concat
      else reduced ()
  in
  try List.map loop dnf |> List.concat |> Option.some with
  | Reduced -> None
  | e -> raise e
;;

let subst_const (is_par : Ident.t -> bool) const vars dnf =
  let new_const = Constr (const#desc, List.map (fun v -> Var v) vars) in
  (* ad hoc substitution TODO () *)
  let map_unify left right =
    match left, right with
    | ident, right when is_par ident -> DUnify (new_const, right)
    | ident, Var v when is_par ident -> DUnify (Var v, new_const)
    | ident, Var v when is_par v -> DUnify (Var ident, new_const)
    | _ -> DUnify (Var left, right)
  in
  let map_disunify left right =
    match left, right with
    | ident, right when is_par ident -> DDisunify (new_const, right)
    | ident, Var v when is_par v -> DDisunify (Var ident, new_const)
    | _ -> DDisunify (Var left, right)
  in
  let map_var = function
    | Var v when is_par v -> new_const
    | x -> x
  in
  dnf
  |> List.map (function
    | DUnify (left, right) -> map_unify left right
    | DDisunify (left, right) -> map_disunify left right
    | DCall (name, values) ->
      let values = List.map map_var values in
      DCall (name, values)
    | DFresh f -> DFresh f)
;;

let mk_new_var_fun vars =
  let count = ref 0 in
  let rec result () =
    let new_name = Printf.sprintf "new_var%d" @@ !count in
    count := !count + 1;
    let ident = Ident.create_local new_name in
    if List.exists (Ident.same ident) vars then result () else ident
  in
  result
;;

let run_per_const par const const_vars dnf =
  let dnf = List.map (subst_const par#by_ident const const_vars) dnf in
  let dnf = dnf |> List.map reduce_const_const |> List.filter_map (fun x -> x) in
  let dnf = List.map Propagate.try_propagate dnf in
  dnf
;;

let run info globals canren =
  let fresh_vars = Canren.get_declared_fresh_vars canren in
  let new_var = mk_new_var_fun (globals @ fresh_vars) in
  let dnf =
    canren |> Dnf.of_canren |> List.map reduce_const_const |> List.filter_map (fun x -> x)
  in
  let consts_info =
    List.map
      (fun (pat, const) ->
        let const_vars = List.init const#arity (fun _ -> new_var ()) in
        pat, const, const_vars)
      info
  in
  let result =
    List.fold_left
      (fun acc (par, const, const_vars) -> run_per_const par const const_vars acc)
      dnf
      consts_info
  in
  let globals =
    globals
    |> List.concat_map (fun glob ->
      List.find_opt (fun (par, _, _) -> par#by_ident glob) consts_info
      |> function
      | Some (_, _, vars) -> vars
      | None -> glob |> Core.List.return)
  in
  let dnf = if Core.List.is_empty result then None else Some result in
  { dnf; globals }
;;
