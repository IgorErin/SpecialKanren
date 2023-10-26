open Typedtree
open Tast_mapper
open Ocanren_patterns
open Patterns
open Sresult

let ( >> ) f g x = f x |> g

let spec_exp par_number p_fun p_par p_var expr =
  let var_variant f s = p_par#exp f && p_var#this s in
  let var_another_variant f s = p_par#exp f && p_var#another s in
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
         Sresult.map ~f @@ run c_rhs
       | _ -> assert false)
    (* conde *)
    | Texp_apply (hd_exp, args) when is_conde hd_exp ->
      (match args with
       | [ (lbf, Some e) ] ->
         let f x = back @@ Texp_apply (hd_exp, [ lbf, Some x ]) in
         Sresult.map ~f @@ run e
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
    (* === *)
    | Texp_apply (hd_exp, args) as d when is_unify hd_exp ->
      (match args with
       | [ (flb, Some fexp); (slb, Some sexp) ] ->
         (match () with
          | () when var_variant fexp sexp || var_variant sexp fexp -> Empty
          | () when var_another_variant fexp sexp || var_another_variant sexp fexp ->
            ReduceConj
          | _ -> Expr expr)
       | _ -> assert false)
    (* =/= *)
    | Texp_apply (hd_exp, args) as d when is_nunify hd_exp ->
      (match args with
       | [ (flb, Some fexp); (slb, Some sexp) ] ->
         (match () with
          | () when var_variant fexp sexp || var_variant sexp fexp -> ReduceConj
          | () when var_another_variant fexp sexp || var_another_variant sexp fexp ->
            Empty
          | _ -> Expr expr)
       | _ -> assert false)
    (* rec call *)
    | Texp_apply (hd_exp, ls) when p_fun#exp hd_exp ->
      let _, arg = List.nth ls par_number in
      (match arg with
       | Some arg ->
         (* 1) if spec param -> delete arg
            2) if spec variant -> delete arg
            TODO() 3) if another variable -> unify that variable with variant conj
            TODO() 4) if another variant -> reduce conj *)
         if p_par#exp arg || p_var#this arg
         then (
           (* delete if equal *)
           let new_args = ls |> List.filteri (fun i _ -> i <> par_number) in
           Expr (back @@ Texp_apply (hd_exp, new_args)))
         else (* if not -> erase conj*)
           ReduceConj
       | None -> failwith "Abstracted over spec paramter. Not implemented.")
    | Texp_apply (hd, args) when is_fresh hd ->
      (* if fresh variable unified with parameter -> TODO()
         1) substitute variant in variable uses
         2) remove fresh vraible creation *)
      (match args with
       | [ (lb, Some e) ] ->
         let f x = back @@ Texp_apply (hd, [ lb, Some x ]) in
         run e |> Sresult.map ~f
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

let spec_fun spec_f spec_par varp source_expr =
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
        spec_exp count spec_f spec_par varp c_rhs |> exp_of_result)
      else (
        let c_rhs = map (count + 1) c_rhs in
        back @@ Texp_function { d with cases = [ { c with c_rhs } ] })
    | _ -> failwith "Function expected."
  in
  map 0 source_expr
;;

let get_variants p =
  let get_type { c_lhs = { pat_type; pat_env; _ }; _ } = pat_type, pat_env in
  let rec loop e =
    match e.exp_desc with
    | Texp_function ({ param; cases = [ ({ c_rhs; _ } as c) ]; _ } as d) ->
      if p param
      then (
        let ty, env = get_type c in
        Some (Typespat.get_cons ty env))
      else loop c_rhs
    | Texp_function _ -> assert false
    | _ -> None
  in
  loop
;;

(* TODO() check that exist only one such function *)

(* 1) find spec_fun in struct_item list
   2) call spec_fun on her arguments
   3) modify fun_predicate since Ident occurse *)
let spec_str_item funp parp str_item =
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
        let cons = get_variants parp#ident vb_expr in
        match cons with
        | Some cons ->
          List.map
            (fun variant ->
              let open Types in
              let varp = Predicate.var_of_constr_desc variant cons in
              let vb_expr = spec_fun funp parp varp vb_expr in
              { vb with vb_expr })
            cons
        | _ -> failwith "param not found")
      else [ vb ]
    in
    let vb_list = vb_list |> List.map map |> List.concat in
    back @@ Tstr_value (recf, vb_list)
  | _ -> str_item
;;

let translate funp parp (t : Typedtree.structure) =
  let iterator = Tast_mapper.default in
  let iterator =
    { iterator with
      structure_item =
        (fun self str ->
          let spec_str_item = spec_str_item funp parp in
          spec_str_item str)
    }
  in
  let t = iterator.structure iterator t in
  let ast = Untypeast.untype_structure t in
  Pprintast.structure Format.std_formatter ast
;;
