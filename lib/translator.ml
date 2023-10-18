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

let rec spec_exp p_var p_variant expr =
  let var_variant f s = p_var#exp f && p_variant#exp s in
  let var_another_variant f s = p_var#exp f && (not @@ p_variant#exp s) in
  let spec_map = spec_exp p_var p_variant in
  let constr_expr_desc d = { expr with exp_desc = d } in
  match expr.exp_desc with
  | Texp_function
      ({ param : Ident.t; cases = [ ({ c_rhs; _ } as c) ]; partial = Total; _ } as d) ->
    if p_var#ident param
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

(* check that exist only one such function *)
let spec_str_item spec_fun spec_exp str_item =
  let back d = { str_item with str_desc = d } in
  match str_item.str_desc with
  | Tstr_value (recf, vb_list) as source ->
    let map ({ vb_expr; _ } as vb) =
      if spec_fun#pat vb.vb_pat
      then (
        let vb_expr =
          spec_exp vb_expr
          |> function
          | Expr x -> x
          | _ -> failwith "Not expr in tstr_value."
        in
        { vb with vb_expr })
      else vb
    in
    let vb_list = vb_list |> List.map map in
    back @@ Tstr_value (recf, vb_list)
  | _ -> str_item
;;

let translate p_par p_var p_fun (t : Typedtree.structure) =
  let iterator = Tast_mapper.default in
  let iterator =
    { iterator with
      structure_item =
        (fun self str ->
          let spec_exp = spec_exp p_par p_var in
          let spec_str_item = spec_str_item p_fun spec_exp in
          spec_str_item str)
    }
  in
  let t = iterator.structure iterator t in
  let ast = Untypeast.untype_structure t in
  Pprintast.structure Format.std_formatter ast
;;
