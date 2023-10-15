open Typedtree
open Asttypes
open Tast_mapper
open Ocanren_patterns
open Patterns

let exp_by_texp_ident ident_list =
  let open Patterns in
  let open Gen in
  let path = list ident_list in
  let path_pattern = Path.match' path in
  let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
  expression exp_desc drop drop drop drop drop
;;

let exp_by_constr_ident name =
  let open Patterns in
  let open Gen in
  let type_desc = Patterns.Types.constructor_description name in
  let exp_desc = Expression_desc.texp_construct drop type_desc drop in
  expression exp_desc drop drop drop drop drop
;;

let exp_by_texp_apply hd arg_list =
  let open Gen in
  let exp_desc = Expression_desc.texp_apply hd arg_list in
  expression exp_desc drop drop drop drop drop
;;

let unify = Gen.(exp_by_texp_ident [ str "OCanren"; str "===" ])
let nunify = Gen.(exp_by_texp_ident [ str "OCanren"; str "=/=" ])
let conj = Gen.(exp_by_texp_ident [ str "OCanren"; str "&&&" ])
let disj = Gen.(exp_by_texp_ident [ str "OCanren"; str "|||" ])

type result =
  | Expr of expression
  | ReduceConj
  | Empty

let reduce_conj (fst : result) (snd : result) cons =
  match fst, snd with
  | Expr fst, Expr snd -> Expr (cons fst snd)
  | Empty, Expr x | Expr x, Empty -> Expr x
  | Empty, Empty -> Empty
  | _ -> ReduceConj
;;

let reduce_disj (fst : result) (snd : result) cons =
  match fst, snd with
  | Expr fst, Expr snd -> Expr (cons fst snd)
  | Empty, Expr x | Expr x, Empty | ReduceConj, Expr x | Expr x, ReduceConj -> Expr x
  | ReduceConj, Empty | Empty, ReduceConj | Empty, Empty -> Empty
  | ReduceConj, ReduceConj -> ReduceConj
;;

let is_conde exp = parse_bool (exp_by_texp_ident Gen.[ str "Ocanren"; str "conde" ]) exp
let is_list_cons = parse_bool Gen.(exp_by_constr_ident @@ str "::")
let is_conj = parse_bool conj
let is_unify = parse_bool unify
let is_nunify = parse_bool nunify
let spec_variant = Gen.(exp_by_texp_ident [ str "is" ])

let spec_var =
  Gen.(exp_by_texp_ident [ str "OCanren"; str "Std"; str "Bool"; str "truo" ])
;;

let is_spec_variant = parse_bool spec_variant
let is_spec_var = parse_bool spec_var
let is_spec_ident ident = String.equal (Ident.name ident) "x"

let rec spec_map : expression -> result =
  fun expr ->
  let constr_expr_desc d = { expr with exp_desc = d } in
  match expr.exp_desc with
  | Texp_function { param : Ident.t; cases = [ ({ c_rhs; _ } as c) ]; partial = Total; _ }
    -> spec_map c_rhs
  (* conde *)
  | Texp_apply (hd_exp, [ (lbf, Some list) ]) when is_conde hd_exp ->
    let result = spec_map list in
    result
    |> (function
    | Expr x -> Expr x
    | _ -> failwith "not implemented")
    (* list cons. disjanction for now *)
  | Texp_construct (ident, typ_desc, [ fst; snd ]) when is_list_cons expr ->
    let fst = spec_map fst in
    let snd = spec_map snd in
    let cons x y = constr_expr_desc @@ Texp_construct (ident, typ_desc, [ x; y ]) in
    reduce_disj fst snd cons
    (* conj *)
  | Texp_apply (hd_exp, [ (flb, Some fexp); (slb, Some sexp) ]) when is_conj hd_exp ->
    let fexp = spec_map fexp in
    let sexp = spec_map sexp in
    let cons x y =
      constr_expr_desc @@ Texp_apply (hd_exp, [ flb, Some x; slb, Some y ])
    in
    reduce_conj fexp sexp cons
  | Texp_apply (hd_exp, [ (flb, Some fexp); (slb, Some sexp) ]) as d when is_unify hd_exp
    ->
    if (is_spec_var fexp && is_spec_variant sexp)
       || (is_spec_variant fexp && is_spec_var sexp)
    then Empty
    else if (is_spec_var fexp && (not @@ is_spec_variant sexp))
            || ((not @@ is_spec_variant fexp) && is_spec_var sexp)
    then ReduceConj
    else Expr (constr_expr_desc d)
  | Texp_apply (hd_exp, [ (flb, Some fexp); (slb, Some sexp) ]) as d when is_nunify hd_exp
    ->
    if (is_spec_var fexp && is_spec_variant sexp)
       || (is_spec_variant fexp && is_spec_var sexp)
    then ReduceConj
    else if (is_spec_var fexp && (not @@ is_spec_variant sexp))
            || ((not @@ is_spec_variant fexp) && is_spec_var sexp)
    then Empty
    else Expr (constr_expr_desc d)
  | x -> Expr (constr_expr_desc x)
;;

let translate (t : Typedtree.structure) =
  let iterator = Tast_mapper.default in
  let iterator =
    { iterator with
      expr =
        (fun self exp ->
          let result = spec_map exp in
          match result with
          | Expr x -> x
          | Empty -> failwith "Empty"
          | ReduceConj -> failwith "ReduceConj")
    }
  in
  let t = iterator.structure iterator t in
  Printtyped.implementation Format.std_formatter t
;;
