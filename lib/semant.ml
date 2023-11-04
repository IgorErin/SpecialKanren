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

module Helpers = struct
  open Parsetree
  open Ast_helper

  let loc = Location.none
  let create_pat ident = Pat.var @@ Location.mknoloc @@ Ident.name ident

  (* copy past from noCanren *)
  let create_apply f = function
    | [] -> f
    | args ->
      let args = List.map (fun a -> Asttypes.Nolabel, a) args in
      (match f.pexp_desc with
       | Pexp_apply (g, args') -> Exp.apply g (args' @ args)
       | _ -> Exp.apply f args)
  ;;

  let create_apply_to_list f arg_list =
    let loc = f.pexp_loc in
    let new_arg =
      List.fold_right (fun x acc -> [%expr [%e x] :: [%e acc]]) arg_list [%expr []]
    in
    create_apply f [ new_arg ]
  ;;

  let create_conj left right = [%expr [%e left] &&& [%e right]]

  let create_disj = function
    | [] -> failwith "Disjunction needs one or more arguments"
    | [ x ] -> x
    | [ x; y ] -> [%expr [%e x] ||| [%e y]]
    | l -> create_apply_to_list [%expr conde] l
  ;;

  let create_fun var body = [%expr fun [%p create_pat var] -> [%e body]]

  let create_fun_closer vars body =
    List.fold_right (fun var acc -> create_fun var acc) vars body
  ;;

  let create_fresh var body = create_apply [%expr Fresh.one] [ create_fun var body ]
  let create_inj expr = [%expr !![%e expr]]

  (* end of copy past *)

  let create_apply f e = [%expr [%e f] [%e e]]
  let create_apply_closer f exps = List.fold_left create_apply f exps
  let create_unify left right = [%expr [%e left] === [%e right]]
  let create_disunify left right = [%expr [%e left] =/= [%e right]]

  open Ast_helper

  let lid_of_ident ident =
    ident
    |> Ident.name
    |> fun x -> Longident.Lident x |> fun x -> Location.mkloc x Location.none
  ;;

  let lid_of_string str = Longident.Lident str |> fun x -> Location.mkloc x Location.none

  let exp_of_value value =
    let rec create_const desc values =
      let exps = List.map inside_constr values in
      (match exps with
       | [] -> None
       | _ :: _ -> Some (Exp.tuple exps))
      |> fun arg ->
      let lid = desc.Types.cstr_name |> lid_of_string in
      Exp.construct lid arg
    and inside_constr = function
      | Var ident -> lid_of_ident ident |> Exp.ident
      | Constr (desc, values) -> create_const desc values
    and loop value =
      match value with
      | Var ident -> lid_of_ident ident |> Exp.ident
      | Constr (desc, values) -> create_const desc values |> create_inj
    in
    loop value
  ;;

  let fresh_closer vars cont =
    List.fold_right (fun var acc -> create_fresh var acc) vars cont
  ;;

  let exp_of_ident ident = lid_of_ident ident |> Exp.ident

  let to_past_one = function
    | DUnify (ident, value) ->
      let left = exp_of_ident ident in
      let right = exp_of_value value in
      create_unify left right
    | DDisunify (ident, value) ->
      let left = exp_of_ident ident in
      let right = exp_of_value value in
      create_disunify left right
    | DCall (ident, values) ->
      let values = List.map exp_of_value values in
      let lident =
        Untypeast.lident_of_path ident |> fun x -> Location.mkloc x Location.none
      in
      let ident = Exp.ident lident in
      create_apply_closer ident values
    | DFresh _ -> failwith "DFresh unexpected for now (TODO)"
  ;;

  let past_of_conj conj =
    let to_past_unify ident value =
      let left = exp_of_ident ident in
      let right = exp_of_value value in
      create_unify left right
    in
    let to_past_disunify ident value =
      let left = exp_of_ident ident in
      let right = exp_of_value value in
      create_disunify left right
    in
    let to_past_call ident values =
      let values = List.map exp_of_value values in
      let lident =
        Untypeast.lident_of_path ident |> fun x -> Location.mkloc x Location.none
      in
      let ident = Exp.ident lident in
      create_apply_closer ident values
    in
    let to_past_one = function
      | DUnify (ident, value) -> to_past_unify ident value
      | DDisunify (ident, value) -> to_past_disunify ident value
      | DCall (ident, values) -> to_past_call ident values
      | DFresh _ -> failwith "DFresh unexpected here (TODO)"
    in
    (* copy past but compiler aware *)
    let to_past_regular current next =
      match current with
      | DUnify (ident, value) ->
        let current = to_past_unify ident value in
        create_conj current next
      | DDisunify (ident, value) ->
        let current = to_past_disunify ident value in
        create_conj current next
      | DCall (ident, values) ->
        let current = to_past_call ident values in
        create_conj current next
      | DFresh vars -> fresh_closer vars next
    in
    match List.rev conj with
    | [] -> failwith "Empty conj TODO()"
    | hd :: tl ->
      let hd = to_past_one hd in
      List.fold_right to_past_regular (List.rev tl) hd
  ;;

  let past_of_dnf dnf = List.map past_of_conj dnf |> create_disj
end

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
  let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
  let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
  Format.fprintf Format.std_formatter "\n%!";
  Format.printf "Global :";
  List.iter (fun x -> Format.printf "%s " @@ Ident.name x) globals;
  pp Format.std_formatter pfst psnd dnf;
  let body = Helpers.past_of_dnf dnf in
  Helpers.create_fun_closer globals body
;;
