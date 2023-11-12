open Value
open Dnf

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

let filter_by_cons (is_par : Ident.t -> bool) const (dnf : _ dnf) =
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
  let lr = Core.List.return in
  let rec loop = function
    | DUnify (Var v, next) | DUnify (next, Var v) -> DUnify (v, next) |> lr
    | DDisunify (Var v, next) | DDisunify (next, Var v) -> DDisunify (v, next) |> lr
    | DCall (ident, values) -> DCall (ident, values) |> lr
    | DFresh _ as f -> f |> lr
    | DUnify (Constr (ld, lv), Constr (rd, rv)) ->
      if Types.may_equal_constr ld rd
      then List.map2 (fun lv rv -> loop @@ DUnify (lv, rv)) lv rv |> List.concat
      else failwith "Constr unify fail"
    | DDisunify (Constr (ld, lv), Constr (rd, rv)) ->
      if Types.may_equal_constr ld rd
      then List.map2 (fun lv rv -> loop @@ DDisunify (lv, rv)) lv rv |> List.concat
      else [] (* is it correct?*)
  in
  List.map loop dnf |> List.concat
;;

let subst_const (is_par : Ident.t -> bool) const vars dnf =
  let new_const = Constr (const#desc, List.map (fun v -> Var v) vars) in
  (* ad hoc substitution TODO () *)
  let map_unify left right : _ cnj =
    (match left, right with
     | ident, right when is_par ident -> DUnify (new_const, right)
     | ident, Var v when is_par ident -> DUnify (Var v, new_const)
     | ident, Var v when is_par v -> DUnify (Var ident, new_const)
     | _ -> DUnify (Var left, right))
    |> Core.List.return
  in
  let map_disunify left right =
    (* some_var =/= par -> some_var =/= Cons (new_var0, new_var1, ..)*)
    (match left, right with
     | ident, right when is_par ident -> DDisunify (new_const, right)
     | ident, Var v when is_par v -> DDisunify (Var ident, new_const)
     | _ -> DDisunify (Var left, right))
    |> Core.List.return
  in
  let map_var = function
    | Var v when is_par v ->
      let vars = List.map (fun x -> Var x) vars in
      Constr (const#desc, vars)
    | x -> x
  in
  dnf
  |> List.map (function
    | DUnify (left, right) -> map_unify left right
    | DDisunify (left, right) -> map_disunify left right
    | DCall (name, values) ->
      let values = List.map map_var values in
      DCall (name, values) |> Core.List.return
    | DFresh f -> Core.List.return @@ DFresh f)
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

module Result = struct
  type fun_hole = (string * value list) option ref
  type spec_info = (int * Types.constructor_description) list

  type fun_info =
    { fname : Ident.t
    ; consts : spec_info
    }

  type hole_info =
    { hfinfo : fun_info
    ; href : fun_hole
    ; hargs : value list
    }

  type ('a, 'b) item =
    | FUnify of 'a * 'b
    | FDisunify of 'a * 'b
    | FFresh of Ident.t list
    | FCallHole of fun_hole
    | FCall of (Path.t * value list)

  type ('a, 'b) conj = ('a, 'b) item list
  type ('a, 'b) dnf = ('a, 'b) conj list

  type ('a, 'b) t =
    { res_info : fun_info
    ; res_dnf : ('a, 'b) dnf
    ; res_deps : hole_info list
    ; res_globals : Ident.t list
    }

  let equal fst snd =
    let consts_same =
      List.length fst.consts = List.length snd.consts
      && List.for_all2
           (fun (fi, fc) (si, sc) ->
             Int.equal fi si && Types.(equal_tag fc.cstr_tag sc.cstr_tag))
           fst.consts
           snd.consts
    in
    Ident.same fst.fname snd.fname && consts_same
  ;;

  let fetch conj =
    let get = function
      | DCall (f_path, args) ->
        let consts =
          args
          |> List.mapi (fun index -> function
            | Constr (desc, _) -> Some (index, desc)
            | Var _ -> None)
          |> List.filter_map (fun x -> x)
        in
        if Core.List.is_empty consts
        then FCall (f_path, args), None
        else (
          let hargs =
            List.concat_map
              (function
               | Constr (_, values) -> values
               | x -> [ x ])
              args
          in
          let href = ref None in
          let fname = Path.head f_path in
          let hfinfo = { fname; consts } in
          FCallHole href, Some { href; hfinfo; hargs })
      | DUnify (left, right) -> FUnify (left, right), None
      | DDisunify (left, right) -> FDisunify (left, right), None
      | DFresh vars -> FFresh vars, None
    in
    let fdnf, funs = List.map get conj |> Core.List.unzip in
    let funs = List.filter_map (fun x -> x) funs in
    fdnf, funs
  ;;

  let process dnf : _ dnf * hole_info list =
    let dnf, funs = List.map fetch dnf |> Core.List.unzip in
    let funs = List.concat funs in
    dnf, funs
  ;;

  let pp f =
    let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
    let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
    List.iter (fun l ->
      Format.printf "\n new disj \n";
      List.iter
        (fun x ->
          Format.printf " && ";
          match x with
          | FUnify (fst, snd) ->
            pfst f fst;
            Format.fprintf f "===";
            psnd f snd
          | FDisunify (fst, snd) ->
            Format.fprintf f "(";
            pfst f fst;
            Format.fprintf f "=/=";
            psnd f snd
          | FCall (ident, values) ->
            Format.fprintf f "%s (" @@ Path.name ident;
            List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
            Format.fprintf f ")"
          | FCallHole rf ->
            (match !rf with
             | Some (name, values) ->
               Format.fprintf f "%s (" name;
               List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
               Format.fprintf f ")"
             | None -> Format.printf "HOLE")
          | FFresh freshs ->
            Format.fprintf f "Fresh (";
            List.iter (fun i -> Format.fprintf f "%s " @@ Ident.name i) freshs;
            Format.fprintf f ")")
        l)
  ;;

  let to_dnf dnf =
    let item = function
      | FUnify (left, right) -> DUnify (left, right)
      | FDisunify (left, right) -> DDisunify (left, right)
      | FFresh fresh -> DFresh fresh
      | FCallHole hole ->
        (match !hole with
         (* TODO *)
         | Some (name, values) -> DCall (Path.Pident (Ident.create_local name), values)
         | None -> failwith "")
      | FCall (path, values) -> DCall (path, values)
    in
    List.map (fun cnj -> List.map item cnj) dnf
  ;;
end

let run_per_const par const const_vars dnf =
  (* reduce conjanctions by const*)
  let dnf = filter_by_cons par#by_ident const dnf in
  (* unwrap spec constructor *)
  let dnf = List.map (subst_const par#by_ident const const_vars) dnf in
  let dnf = List.map reduce_const_const dnf in
  let dnf = List.map Propagate.try_propagate dnf in
  dnf
;;

let run info fname globals canren =
  let fresh_vars = Canren.get_declared_fresh_vars canren in
  let new_var = mk_new_var_fun (globals @ fresh_vars) in
  let dnf = Dnf.of_canren canren in
  let dnf = List.map reduce_const_const dnf in
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
  let res_dnf, res_deps = result |> Result.process in
  let consts = info |> List.map (fun (par, var) -> par#number, var#desc) in
  Result.{ res_dnf; res_deps; res_info = { fname; consts }; res_globals = globals }
;;
