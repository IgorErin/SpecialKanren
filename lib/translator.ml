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

let rec spec_exp par_number p_fun p_par p_var expr =
  let var_variant f s = p_par#exp f && p_var#exp s in
  let var_another_variant f s = p_par#exp f && (not @@ p_var#exp s) in
  let spec_exp = spec_exp par_number p_fun p_par p_var in
  let back d = { expr with exp_desc = d } in
  let exp_of_result = function
    | Expr x -> x
    | _ -> failwith "Not implemented"
  in
  match expr.exp_desc with
  | Texp_function ({ param; cases = [ ({ c_rhs; _ } as c) ]; partial = Total; _ } as d) ->
    let result = spec_exp c_rhs in
    let c_rhs = exp_of_result result in
    let exp_desc = Texp_function { d with cases = [ { c with c_rhs } ] } in
    Expr (back exp_desc)
  (* conde *)
  | Texp_apply (hd_exp, [ (lbf, Some e) ]) when is_conde hd_exp ->
    let x = spec_exp e |> exp_of_result in
    Expr (back @@ Texp_apply (hd_exp, [ lbf, Some x ]))
    (* list cons. disjanction for now *)
  | Texp_construct (ident, typ_desc, [ fst; snd ]) when is_list_cons expr ->
    let fst = spec_exp fst in
    let snd = spec_exp snd in
    let cons x y = back @@ Texp_construct (ident, typ_desc, [ x; y ]) in
    reduce_disj fst snd cons
    (* conj *)
    (* check type of list. should be logic ... Add (|||) predicate*)
  | Texp_apply (hd_exp, [ (flb, Some fexp); (slb, Some sexp) ]) when is_conj hd_exp ->
    let fexp = spec_exp fexp in
    let sexp = spec_exp sexp in
    let cons x y = back @@ Texp_apply (hd_exp, [ flb, Some x; slb, Some y ]) in
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
  | Texp_apply (hd_exp, ls) when p_fun#exp hd_exp ->
    (* self rec call. remove argument. TODO (if another variant in argument. We should erase conj) *)
    assert (List.length ls >= par_number + 1);
    (* since count start from 0 *)
    let new_args = ls |> List.filteri (fun i _ -> i <> par_number) in
    Expr (back @@ Texp_apply (hd_exp, new_args))
  | Texp_apply (hd, ls) when is_fresh hd ->
    List.map
      (fun (lb, e) ->
        Option.map (fun x -> spec_exp x |> exp_of_result) e |> fun x -> lb, x)
      ls
    |> fun ls -> Expr (back @@ Texp_apply (hd, ls))
  | _ -> Expr expr
;;

(* process spec fun formal arguments till spec_param
   1) find spec param nubmer
   2) reduce sepc formal parameter
   3) call spec_exp with modified spec_f predicate object with info about (1)*)
let spec_fun spec_f spec_par spec_var soruce_expr =
  let exp_of_result = function
    | Expr x -> x
    | _ -> failwith "spec_fun. Not Expr"
  in
  let rec map count expr =
    let back d = { expr with exp_desc = d } in
    match expr.exp_desc with
    | Texp_function ({ param; cases = [ ({ c_rhs; _ } as c) ]; _ } as d) ->
      if spec_par#ident param
      then (
        let spec_par = Predicate.par_of_ident param in
        (* reduce. juct skip for now *)
        let spec_f = spec_f in
        spec_exp count spec_f spec_par spec_var c_rhs |> exp_of_result)
      else (
        let c_rhs = map (count + 1) c_rhs in
        back @@ Texp_function { d with cases = [ { c with c_rhs } ] })
    | _ -> failwith "Function expected."
  in
  map 0 soruce_expr
;;

(* TODO() check that exist only one such function *)

(* 1) find spec_fun in struct_item list
   2) call spec_fun on her arguments
   3) modify fun_predicate since Ident occurse *)
let spec_str_item funp parp varp str_item =
  let back d = { str_item with str_desc = d } in
  let ident_of_pattern = function
    | { pat_desc = Tpat_var (id, _); _ } -> id
    | _ -> failwith "Var expected."
  in
  match str_item.str_desc with
  | Tstr_value (recf, vb_list) as source ->
    let map ({ vb_expr; _ } as vb) =
      if funp#pat vb.vb_pat
      then (
        let fun_ident = ident_of_pattern vb.vb_pat in
        let funp = Predicate.fun_of_ident fun_ident in
        let vb_expr = spec_fun funp parp varp vb_expr in
        { vb with vb_expr })
      else vb
    in
    let vb_list = vb_list |> List.map map in
    back @@ Tstr_value (recf, vb_list)
  | _ -> str_item
;;

let translate funp parp varp (t : Typedtree.structure) =
  let iterator = Tast_mapper.default in
  let iterator =
    { iterator with
      structure_item =
        (fun self str ->
          let spec_str_item = spec_str_item funp parp varp in
          spec_str_item str)
    }
  in
  let t = iterator.structure iterator t in
  let ast = Untypeast.untype_structure t in
  Pprintast.structure Format.std_formatter ast
;;
