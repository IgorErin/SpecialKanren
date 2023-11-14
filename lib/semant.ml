open Value
open Dnf

type ('a, 'b) result =
  { globals : Ident.t list
  ; dnf : ('a, 'b) dnf
  }

module Names = struct
  let travers_graph (cnj : _ cnj) =
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
        | Var ident as v -> v :: all_values_by_ident ident
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

  let is_used ident conj =
    let rec used_value = function
      | Var v -> Ident.same ident v
      | Constr (_, values) -> List.exists used_value values
    in
    List.exists
      (function
       | DDisunify (_, Constr (_, values))
       | DUnify (_, Constr (_, values))
       | DCall (_, values) -> List.exists used_value values
       | _ -> false)
      conj
  ;;

  let is_redudant ident conj =
    let info = travers_graph conj in
    let opt_values = List.find_opt (fun (fst, _) -> Ident.same ident fst) info in
    let is_used = is_used ident conj in
    let same = Ident.same ident in
    (* Format.printf " is_used: %b " is_used; *)
    (match opt_values with
     | Some (_, values) ->
       let vars, consts = Core.List.partition_map ~f:Value.partition values in
       let vars = List.filter (fun x -> x |> same |> not) vars in
       let only_ground =
         consts
         |> List.concat_map (fun (_, values) -> values)
         |> List.for_all Value.is_ground
       in
       let is_empty = Core.List.is_empty vars in
       is_empty && only_ground
     | None -> true)
    |> ( && ) (not is_used)
  ;;

  let reduce ident conj =
    let same = Ident.same ident in
    List.filter_map
      (function
       | DUnify (id, Var v) when same id || same v -> None
       | DDisunify (id, Var v) when same id || same v -> None
       | DUnify (id, Constr _) when same id -> None
       | DDisunify (id, Constr _) when same id -> None
       | DFresh freshs ->
         Option.some @@ DFresh (List.filter (fun x -> x |> same |> not) freshs)
       | x -> Some x)
      conj
  ;;

  let get_freshs list =
    list
    |> List.fold_left
         (fun acc -> function
           | DFresh freshs -> freshs @ acc
           | _ -> acc)
         []
  ;;

  let try_reduce cnj =
    let freshs = get_freshs cnj in
    List.fold_left
      (fun cnj fresh -> if is_redudant fresh cnj then reduce fresh cnj else cnj)
      cnj
      freshs
  ;;

  let remove_empty_fresh cnj =
    cnj
    |> List.filter_map (function
      | DFresh [] -> None
      | x -> Some x)
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
  let dnf = List.map Names.try_propagate dnf in
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
    |> List.map Names.try_reduce
    |> List.map Names.remove_empty_fresh
  in
  let globals =
    globals
    |> List.concat_map (fun glob ->
      List.find_opt (fun (par, _, _) -> par#by_ident glob) consts_info
      |> function
      | Some (_, _, vars) -> vars
      | None -> glob |> Core.List.return)
  in
  { dnf = result; globals }
;;
