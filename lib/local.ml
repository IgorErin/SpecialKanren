open Value
open Dnf

type ('a, 'b) result =
  { globals : Ident.t list
  ; dnf : ('a, 'b) dnf
  }

module Utils = struct
  module IdentMap = Map.Make (Ident)

  let mk_new_var_fun vars =
    let count = ref 0 in
    let rec result () =
      let new_name = Printf.sprintf "constarg%d" @@ !count in
      count := !count + 1;
      let ident = Ident.create_local new_name in
      if List.exists (Ident.same ident) vars then result () else ident
    in
    result
  ;;

  let travers_unify_graph (cnj : _ cnj) =
    let info =
      List.map
        (function
         | DUnify (ident, Var v) -> [ ident, Var v; v, Var ident ]
         | DUnify (ident, (Constr _ as c)) -> [ ident, c ]
         | _ -> [])
        cnj
      |> List.concat
    in
    let names = List.map fst info |> Core.List.dedup_and_sort ~compare:Ident.compare in
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
        if Core.List.is_empty diff then acc else loop (acc @ diff) new_front
      in
      let result = loop [] [ Var name ] in
      name, result
    in
    names |> List.map get_values
  ;;
end

exception Reduced

module Passes = struct
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

  let to_value_value conj =
    let loop = function
      | DUnify (left, right) -> DUnify (Var left, right)
      | DDisunify (left, right) -> DDisunify (Var left, right)
      | DFresh freshs -> DFresh freshs
      | DCall (name, vals) -> DCall (name, vals)
    in
    List.map loop conj
  ;;

  let clean_taut cnj =
    cnj
    |> to_value_value
    |> List.filter_map (function
      | DUnify (left, right) when Value.same left right -> None
      | x -> Some x)
    |> reduce_const_const
  ;;
end

module Propagate = struct
  type infer =
    | Delete of Ident.t list
    | Subst of Value.t * Ident.t list

  let infer_result is_global values =
    let vars, consts = Core.List.partition_map ~f:Value.partition values in
    match vars, consts with
    | [], [] -> None
    | _ :: _, [] ->
      let globals = List.filter is_global vars in
      let fresh_vars = List.filter (fun x -> not @@ is_global x) vars in
      (match globals with
       | [] -> Delete fresh_vars
       | hd :: _ -> Subst (Var hd, fresh_vars))
      |> Option.some
    (* TODO take more ground etc *)
    | _, fst :: _ ->
      let ground = consts |> List.map Value.constr |> List.filter Value.is_ground in
      let fresh_vars = List.filter (fun x -> not @@ is_global x) vars in
      (match ground with
       | [] -> Subst (Constr fst, fresh_vars)
       | hd :: _ -> Subst (hd, fresh_vars))
      |> Option.some
  ;;

  let eq_classes info =
    info
    |> List.fold_left
         (fun acc (fst, values) ->
           let var = Var fst in
           let acc_values = List.concat_map snd acc in
           if List.exists (Value.same var) acc_values then acc else (fst, values) :: acc)
         []
    |> List.map snd
  ;;

  type usage =
    | Dis
    | Call
    | Cons

  let get_usages (conj : (Ident.t, Value.t) cnj) ident =
    let same = Ident.same ident in
    let rec used_value = function
      | Var v -> same v
      | Constr (_, values) -> List.exists used_value values
    in
    conj
    |> List.filter_map (function
      | (DDisunify (ident, Constr _) | DDisunify (_, Var ident)) when same ident ->
        Dis |> Option.some
      | (DDisunify (_, Constr (_, values)) | DUnify (_, Constr (_, values)))
        when List.exists used_value values -> Cons |> Option.some
      | DCall (_, values) when List.exists used_value values -> Call |> Option.some
      | _ -> None)
  ;;

  let app conj result =
    let delete ls ident =
      let same = Ident.same ident in
      ls
      |> List.filter_map (function
        | (DUnify (ident, Constr _) | DUnify (_, Var ident)) when same ident -> None
        | DUnify (ident, Var _) when same ident -> None
        | x -> Some x)
    in
    let subst value vars =
      let same n = List.exists (Ident.same n) vars in
      let rec subst_value = function
        | Var ident when same ident -> value
        | Constr (desc, values) ->
          let values = List.map subst_value values in
          Constr (desc, values)
        | x -> x
      in
      function
      | DUnify (left, right) -> DUnify (subst_value left, subst_value right)
      | DDisunify (left, right) -> DDisunify (subst_value left, subst_value right)
      | DCall (name, values) -> DCall (name, List.map subst_value values)
      | DFresh names -> DFresh names
    in
    match result with
    | Delete vars ->
      let usages = List.concat_map (get_usages conj) vars in
      (if Core.List.is_empty usages then List.fold_left delete conj vars else conj)
      |> Passes.to_value_value
    | Subst (value, names) ->
      conj |> Passes.to_value_value |> List.map (subst value names)
  ;;

  let run is_global conj =
    let info = Utils.travers_unify_graph conj in
    let classes = eq_classes info in
    let results = List.filter_map (infer_result is_global) classes in
    let app conj result =
      let map conj = app conj result |> Passes.reduce_const_const in
      Option.bind conj map
    in
    List.fold_left app (Some conj) results
  ;;
end

module Reduce = struct
  let is_used ident (conj : (Ident.t, Value.t) cnj) =
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
    let info = Utils.travers_unify_graph conj in
    let opt_values = List.find_opt (fun (fst, _) -> Ident.same ident fst) info in
    let is_used = is_used ident conj in
    let same = Ident.same ident in
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

let run_per_const par const const_vars dnf =
  let dnf = List.map (Passes.subst_const par#by_ident const const_vars) dnf in
  let dnf = dnf |> List.filter_map Passes.reduce_const_const in
  dnf
;;

let fuse_fresh conj =
  conj
  |> List.fold_left
       (fun (facc, dacc) -> function
         | DFresh freshs -> List.rev freshs @ facc, dacc
         | x ->
           if Core.List.is_empty facc
           then [], x :: dacc
           else [], [ x; DFresh (List.rev facc) ] @ dacc)
       ([], [])
  |> snd
  |> List.rev
;;

let run ~info ~freshs ~globals ~dnf =
  let new_var = Utils.mk_new_var_fun (globals @ freshs) in
  let dnf = dnf |> List.map Passes.reduce_const_const |> List.filter_map (fun x -> x) in
  let consts_info =
    List.map
      (fun (pat, const) ->
        let const_vars = List.init const#arity (fun _ -> new_var ()) in
        pat, const, const_vars)
      info
  in
  let globals =
    globals
    |> List.concat_map (fun glob ->
      List.find_opt (fun (par, _, _) -> par#by_ident glob) consts_info
      |> function
      | Some (_, _, vars) -> vars
      | None -> glob |> Core.List.return)
  in
  let result =
    let is_global x = List.exists (Ident.same x) globals in
    List.fold_left
      (fun acc (par, const, const_vars) -> run_per_const par const const_vars acc)
      dnf
      consts_info
    |> List.filter_map (Propagate.run is_global)
    |> List.filter_map Passes.clean_taut
    |> List.map Reduce.try_reduce
    |> List.map Reduce.remove_empty_fresh
    |> List.map fuse_fresh
  in
  { dnf = result; globals }
;;
