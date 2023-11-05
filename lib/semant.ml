open Value
open Dnf

let filter_by_cons (is_par : Ident.t -> bool) const (dnf : _ dnf list list) =
  dnf
  |> List.filter (fun conj ->
    conj
    |> List.exists (function
      | DUnify (ident, Constr (desc, _)) when is_par ident && const#cd_another desc ->
        true
      | _ -> false)
    |> not)
;;

let reduce_const_const dnf =
  let reduce = function
    | DUnify (Var v, next) | DUnify (next, Var v) -> DUnify (v, next)
    | DDisunify (Var v, next) | DDisunify (next, Var v) -> DDisunify (v, next)
    | DDisunify (Constr _, Constr _) | DUnify (Constr _, Constr _) ->
      failwith "Const const unif not implemented"
    | DCall (ident, values) -> DCall (ident, values)
    | DFresh _ as f -> f
  in
  List.map reduce dnf
;;

let unwrap_const (is_par : Ident.t -> bool) const vars dnf =
  (* ad hoc substitution TODO () *)
  let map_unify left right : _ dnf list =
    (* par === Cons (var1, var2, ...) -> new_var1 === var1 &&& new_var2 === var2 &&& ...*)
    match left, right with
    | ident, Constr (desc, values) when is_par ident && const#by_desc desc ->
      assert (List.length values = List.length vars);
      List.map2 (fun value var -> DUnify (var, value)) values vars
      (* some_var === par -> some_var === Cons (new_var0, new_var1, ..)*)
    | ident, Var v when is_par ident && (not @@ is_par v) ->
      let vars = List.map (fun v -> Var v) vars in
      DUnify (v, Constr (const#desc, vars)) |> Core.List.return
    | ident, Var v when is_par v && (not @@ is_par ident) ->
      let vars = List.map (fun v -> Var v) vars in
      DUnify (ident, Constr (const#desc, vars)) |> Core.List.return
    | _ -> DUnify (left, right) |> Core.List.return
  in
  dnf
  |> List.map (function
    | DUnify (left, right) -> map_unify left right
    | x -> Core.List.return x)
  |> List.concat
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

type trares =
  | Delete of Ident.t list
  | Promote of Ident.t * const

let travers_graph names cnj =
  let info =
    List.map
      (function
       | DUnify (ident, Var v) -> [ ident, Var v; v, Var ident ]
       | DUnify (ident, (Constr _ as c)) -> [ ident, c ]
       | _ -> [])
      cnj
    |> List.concat
  in
  let get_values name =
    let isame = Ident.same in
    let vsame = Value.same in
    let all_values_by_ident soruce_name =
      List.filter_map
        (fun (name, value) -> if isame soruce_name name then Some value else None)
        info
    in
    let all_valeus_by_value = function
      | Var ident -> all_values_by_ident ident
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
    loop [] [ Var name ]
  in
  let infer_value values =
    let vars, constrs = values |> Core.List.partition_map ~f:Value.partition in
    match vars, constrs with
    | vars, [] -> Delete vars
    | [ name ], [ const ] ->
      let value = Constr const in
      if Value.is_ground value then Promote (name, const) else failwith "Not implemented"
    (* assume that items unique *)
    | _, _ :: _ -> failwith "Mor than one constr. Not implemented"
  in
  List.map get_values names |> List.map infer_value
;;

let substitute info conj =
  let delete_vars names =
    let to_delete name = List.exists (Ident.same name) names in
    (function
     | DUnify (ident, Var v) as d ->
       if to_delete ident || to_delete v then None else Some d
     | x -> Some x)
    |> List.filter_map
  in
  List.fold_left
    (fun conj -> function
      | Promote _ -> conj (*TODO*)
      | Delete names -> delete_vars names conj)
    conj
    info
;;

let reduce_vars global dnf =
  let get_vars conj =
    let rec in_val = function
      | Var v -> Core.List.return v
      | Constr (_, values) -> List.map in_val values |> List.concat
    in
    let in_dnf = function
      | DUnify (ident, value) | DDisunify (ident, value) -> ident :: in_val value
      | DCall (_, values) -> List.map in_val values |> List.concat
      | DFresh _ -> []
    in
    List.fold_left (fun acc dnf -> in_dnf dnf @ acc) [] conj
  in
  List.map
    (fun cnj ->
      let vars = get_vars cnj in
      let is_used name = List.exists (Ident.same name) vars in
      let filter = List.filter is_used in
      List.map
        (function
         | DFresh vars -> DFresh (filter vars)
         | x -> x)
        cnj
      |> fun result -> result, vars)
    dnf
  |> fun all ->
  let conjs, vars = Core.List.unzip all in
  let globals =
    let all_vars = List.concat vars in
    let filter name = List.exists (Ident.same name) all_vars in
    List.filter filter global
  in
  conjs, globals
;;

let process par const exp =
  let globals, tree = Canren.of_tast exp in
  let fresh_vars = Canren.get_declared_fresh_vars @@ Option.get tree in
  let get_new_var = mk_new_var_fun (globals @ fresh_vars) in
  let vars = List.init const#arity (fun _ -> get_new_var ()) in
  let globals =
    List.map (fun var -> if Ident.same var par#ident then vars else [ var ]) globals
    |> List.concat
  in
  let result =
    tree
    |> Option.get
    |> of_canren
    |> List.map reduce_const_const
    |> filter_by_cons par#by_ident const
    |> List.map (unwrap_const par#by_ident const vars)
  in
  let dnf, globals = reduce_vars globals result in
  let _ =
    fun () ->
    (* if verbose ... TODO() *)
    let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
    let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
    Format.fprintf Format.std_formatter "\n%!";
    Format.printf "Global :";
    List.iter (fun x -> Format.printf "%s " @@ Ident.name x) globals;
    pp Format.std_formatter pfst psnd dnf
  in
  let body = Dnf_past.past_of_dnf dnf in
  Dnf_past.create_fun_closer globals body
;;
