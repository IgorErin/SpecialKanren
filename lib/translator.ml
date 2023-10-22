open Typedtree
open Tast_mapper
open Ocanren_patterns
open Patterns

module Result = struct
  type result =
    | Expr of expression
    | ReduceConj
    | Empty

  let map ~f = function
    | Expr x -> Expr (f x)
    | Empty -> Empty
    | ReduceConj -> ReduceConj
  ;;

  let bind ~f = function
    | Expr x -> f x
    | Empty -> Empty
    | ReduceConj -> ReduceConj
  ;;

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

  let get = function
    | Expr x -> x
    | _ -> failwith "Expr expected."
  ;;

  let is_reduce_conj = function
    | ReduceConj -> true
    | _ -> false
  ;;
end

open Result

let ( >> ) f g x = f x |> g

let spec_exp par_number p_fun p_par p_var expr =
  let var_variant f s = p_par#exp f && p_var#exp s in
  let var_another_variant f s = p_par#exp f && (not @@ p_var#exp s) in
  let rec run par_number p_fun p_par p_var expr =
    let back d = { expr with exp_desc = d } in
    let run = run par_number p_fun p_par p_var in
    match expr.exp_desc with
    (* if many cases TODO() *)
    | Texp_function ({ param; cases; partial = Total; _ } as d) ->
      (match cases with
       | [ ({ c_rhs; _ } as c) ] ->
         let f c_rhs =
           let exp_desc = Texp_function { d with cases = [ { c with c_rhs } ] } in
           back exp_desc
         in
         Result.map ~f @@ run c_rhs
       | _ -> assert false)
    (* conde *)
    | Texp_apply (hd_exp, args) when is_conde hd_exp ->
      (match args with
       | [ (lbf, Some e) ] ->
         let f x = back @@ Texp_apply (hd_exp, [ lbf, Some x ]) in
         Result.map ~f @@ run e
       | _ -> assert false)
      (* list cons. disjanction for now *)
    | Texp_construct (ident, typ_desc, args) when is_disj expr ->
      (match args with
       | [ fst; snd ] ->
         let fst = run fst in
         let snd = run snd in
         let cons x y = back @@ Texp_construct (ident, typ_desc, [ x; y ]) in
         reduce_disj fst snd cons
       | _ -> assert false)
      (* conj *)
      (* check type of list. should be logic ...*)
    | Texp_apply (hd_exp, args) when is_conj hd_exp ->
      (match args with
       | [ (flb, Some fexp); (slb, Some sexp) ] ->
         let fexp = run fexp in
         let sexp = run sexp in
         let cons x y = back @@ Texp_apply (hd_exp, [ flb, Some x; slb, Some y ]) in
         reduce_conj fexp sexp cons
       | _ -> assert false)
    | Texp_apply (hd_exp, args) as d when is_unify hd_exp ->
      (match args with
       | [ (flb, Some fexp); (slb, Some sexp) ] ->
         if var_variant fexp sexp || var_variant sexp fexp
         then Empty
         else if var_another_variant fexp sexp || var_another_variant sexp fexp
         then ReduceConj
         else Expr expr
       | _ -> assert false)
    | Texp_apply (hd_exp, args) as d when is_nunify hd_exp ->
      (match args with
       | [ (flb, Some fexp); (slb, Some sexp) ] ->
         if var_variant fexp sexp || var_variant sexp fexp
         then ReduceConj
         else if var_another_variant fexp sexp || var_another_variant sexp fexp
         then Empty
         else Expr expr
       | _ -> assert false)
    | Texp_apply (hd_exp, ls) when p_fun#exp hd_exp ->
      let _, arg = List.nth ls par_number in
      (* self rec call. remove argument. TODO (if another variant in argument. We should erase conj) *)
      (match arg with
       | Some arg ->
         if p_par#exp arg
         then (
           (* delete if equal *)
           let new_args = ls |> List.filteri (fun i _ -> i <> par_number) in
           Expr (back @@ Texp_apply (hd_exp, new_args)))
         else (* if not -> erase conj*)
           ReduceConj
       | None -> failwith "Abstracted over spec paramter. Not implemented.")
    | Texp_apply (hd, args) when is_fresh hd ->
      (match args with
       | [ (lb, Some e) ] ->
         let f x = back @@ Texp_apply (hd, [ lb, Some x ]) in
         run e |> Result.map ~f
       | _ -> assert false)
    (* paramter -> variant *)
    | _ when p_par#exp expr -> failwith "not implemented!" (* TODO substitute variant *)
    | _ -> Expr expr
  in
  run par_number p_fun p_par p_var expr
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
