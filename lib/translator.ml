open Typedtree
open Asttypes
open Tast_mapper
open Ocanren_patterns
open Patterns

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
(* add: ctx with level info, function info, spec only special function etc *)
let rec spec_map spec_var spec_variant expr =
  let var_variant f s = spec_var#exp f && spec_variant#exp s in
  let var_another_variant f s = spec_var#exp f && (not @@ spec_variant#exp s) in
  let spec_map = spec_map spec_var spec_variant in
  let constr_expr_desc d = { expr with exp_desc = d } in
  match expr.exp_desc with
  | Texp_function
      ({ param : Ident.t; cases = [ ({ c_rhs; _ } as c) ]; partial = Total; _ } as d) ->
    if spec_var#ident param
    then spec_map c_rhs
    else
      spec_map c_rhs
      |> (function
      | Expr c_rhs ->
        let cases = [ { c with c_rhs } ] in
        let exp_desc = Texp_function { d with cases } in
        Expr (constr_expr_desc exp_desc)
      | _ -> failwith "not implemented")
  (* conde *)
  | Texp_apply (hd_exp, [ (lbf, Some e) ]) when is_conde hd_exp ->
    spec_map e
    |> (function
    | Expr x -> Expr (constr_expr_desc @@ Texp_apply (hd_exp, [ lbf, Some x ]))
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
    (* what if (x === ro(spec_var)) TODO())*)
  | Texp_apply (hd_exp, [ (flb, Some fexp); (slb, Some sexp) ]) as d when is_unify hd_exp
    ->
    if var_variant fexp sexp || var_variant sexp fexp
    then Empty
    else if var_another_variant fexp sexp || var_another_variant sexp fexp
    then ReduceConj
    else Expr expr
  | Texp_apply (hd_exp, [ (flb, Some fexp); (slb, Some sexp) ]) as d when is_nunify hd_exp
    ->
    if var_variant fexp sexp || var_variant sexp fexp
    then ReduceConj
    else if var_another_variant fexp sexp || var_another_variant sexp fexp
    then Empty
    else Expr expr
  | _ -> Expr expr
;;

let translate spec_var spec_variant (t : Typedtree.structure) =
  let iterator = Tast_mapper.default in
  let iterator =
    { iterator with
      expr =
        (fun self exp ->
          let result = spec_map spec_var spec_variant exp in
          match result with
          | Expr x -> x
          | Empty -> failwith "Empty"
          | ReduceConj -> failwith "ReduceConj")
    }
  in
  let t = iterator.structure iterator t in
  let ast = Untypeast.untype_structure t in
  Pprintast.structure Format.std_formatter ast
;;
