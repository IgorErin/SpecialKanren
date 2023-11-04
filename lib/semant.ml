module IdentMap = struct
  include Map.Make (Ident)
end

open Canren
open Canren.Value

type ('a, 'b) dnf =
  | DUnify of ('a * 'b)
  | DDisunify of ('a * 'b)
  | DCall of (Path.t * Value.value list)
  | DFresh of Ident.t list

let of_canren canren =
  let wrap x = x |> Core.List.return |> Core.List.return in
  let rec loop = function
    | Conj (left, right) ->
      Core.List.cartesian_product (loop left) (loop right)
      |> List.map (fun (fst, snd) -> fst @ snd)
    | Disj (left, right) -> loop left @ loop right
    | Unify (left, right) -> DUnify (left, right) |> wrap
    | Disunify (left, right) -> DDisunify (left, right) |> wrap
    | Call (name, values) -> DCall (name, values) |> wrap
    | Fresh (freshs, next) -> List.map (fun item -> [ DFresh freshs ] @ item) @@ loop next
  in
  loop canren
;;

let pp f pfst psnd =
  List.iter (fun l ->
    Format.printf "\n new disj \n";
    List.iter
      (fun x ->
        Format.printf " && ";
        match x with
        | DUnify (fst, snd) ->
          pfst f fst;
          Format.fprintf f "===";
          psnd f snd
        | DDisunify (fst, snd) ->
          Format.fprintf f "(";
          pfst f fst;
          Format.fprintf f "=/=";
          psnd f snd
        | DCall (ident, values) ->
          Format.fprintf f "%s (" @@ Path.name ident;
          List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
          Format.fprintf f ")"
        | DFresh freshs ->
          Format.fprintf f "Fresh (";
          List.iter (fun i -> Format.fprintf f "%s " @@ Ident.name i) freshs;
          Format.fprintf f ")")
      l)
;;

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
  let global_vars, tree = Canren.of_tast exp in
  let fresh_vars = Canren.get_declared_fresh_vars @@ Option.get tree in
  let get_new_var = mk_new_var_fun (global_vars @ fresh_vars) in
  let vars = List.init const#arity (fun _ -> get_new_var ()) in
  let result =
    tree
    |> Option.get
    |> of_canren
    |> List.map reduce_const_const
    |> filter_by_cons par#by_ident const
    |> List.map (unwrap_const par#by_ident const vars)
  in
  let dnf, _ = reduce_vars global_vars result in
  let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
  let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
  Format.fprintf Format.std_formatter "\n%!";
  pp Format.std_formatter pfst psnd dnf
;;
