open Typedtree
open Tast_mapper
open Ocanren_patterns
open Patterns
open Sresult

(* TODO() check that exist only one such function *)

(* 1) find spec_fun in struct_item list
   2) call spec_fun on her arguments
   3) modify fun_predicate with fun name Ident *)
let spec_str_item funp parp varp str_item =
  let open Ast_helper in
  let open Typedtree in
  let var_variant f s = parp#exp f && varp#this s in
  let var_another_variant f s = parp#exp f && varp#another s in
  let untyp_exp = Untypeast.untype_expression in
  let spec_exp exp =
    let rec loop exp : Parsetree.expression sresult =
      match exp.exp_desc with
      | Texp_function { param; cases = [ { c_rhs; _ } ]; _ } when parp#ident param ->
        loop c_rhs
      | Texp_function { arg_label; param; cases = [ { c_lhs; c_guard; c_rhs } ]; partial }
        ->
        let open Untypeast in
        let c_lhs = untype_pattern c_lhs in
        let c_guard = Option.map untype_expression c_guard in
        let c_rhs = loop c_rhs in
        (* TODO *)
        (match c_rhs with
         | Expr c_rhs -> Exp.fun_ arg_label c_guard c_lhs c_rhs |> fun x -> Expr x
         | _ -> c_rhs)
      | Texp_function { arg_label; param; cases; partial } ->
        List.map
          (fun { c_lhs; c_guard; c_rhs } ->
            let open Untypeast in
            let c_lhs = untype_pattern c_lhs in
            let c_guard = Option.map untype_expression c_guard in
            let c_rhs = Sresult.get @@ loop c_rhs in
            (* TODO *)
            Exp.case c_lhs ?guard:c_guard c_rhs)
          cases
        |> Exp.function_
        |> fun x -> Expr x
        (* conde *)
      | Texp_apply (hd_exp, args) when is_conde hd_exp ->
        (match args with
         | [ (lbf, Some e) ] ->
           let hd_exp = Untypeast.untype_expression hd_exp in
           let f next = Exp.apply hd_exp [ lbf, next ] in
           let loc = Location.none in
           Sresult.map ~f @@ (loop e |> Sresult.with_default [%expr []])
         | _ -> assert false)
      (* list cons *)
      | Texp_construct (ident, typ_desc, args) when is_disj exp ->
        (match args with
         | [ fst; snd ] ->
           let fst = loop fst in
           let snd = loop snd in
           let result = Exp.construct ident in
           let f x y =
             let loc = Location.none in
             [%expr [%e x] :: [%e y]]
           in
           reduce_disj fst snd f
         | _ -> assert false)
        (* conj *)
      | Texp_apply (hd_exp, args) when is_conj hd_exp ->
        (match args with
         | [ (flb, Some fexp); (slb, Some sexp) ] ->
           let fexp = loop fexp in
           let sexp = loop sexp in
           let cons x y =
             let loc = Location.none in
             [%expr [%e x] &&& [%e y]]
           in
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
            | _ -> Expr (untyp_exp exp))
         | _ -> assert false)
      (* =/= *)
      | Texp_apply (hd_exp, args) as d when is_nunify hd_exp ->
        (match args with
         | [ (flb, Some fexp); (slb, Some sexp) ] ->
           (match () with
            | () when var_variant fexp sexp || var_variant sexp fexp -> ReduceConj
            | () when var_another_variant fexp sexp || var_another_variant sexp fexp ->
              Empty
            | _ -> Expr (untyp_exp exp))
         | _ -> assert false)
      (* rec call *)
      | Texp_apply (hd_exp, ls) when funp#exp hd_exp ->
        let _, arg = List.nth ls parp#number in
        (match arg with
         | Some arg ->
           (* 1) if spec param -> delete arg
              2) if spec variant -> delete arg
              TODO() 3) if another variable -> unify that variable with variant conj
              TODO() 4) if another variant -> reduce conj *)
           (match () with
            | () when parp#exp arg || varp#this arg ->
              (* delete if equal number *)
              let new_args =
                ls
                |> List.filteri (fun i _ -> i <> parp#number)
                |> List.filter_map (fun (lb, vl) ->
                  Option.map (fun x -> lb, Sresult.get @@ loop x) vl)
              in
              let hd = untyp_exp hd_exp in
              Expr (Exp.apply hd new_args)
            | _ -> (* if not -> erase conj*) ReduceConj)
         | None -> failwith "Abstracted over spec paramter. Not implemented.")
      | Texp_apply (hd, args) when is_fresh hd ->
        (* if fresh variable unified with parameter -> TODO()
           1) substitute variant in variable uses
           2) remove fresh vraible creation *)
        (match args with
         | [ (lb, Some e) ] ->
           let f x = Exp.apply (untyp_exp hd) [ lb, x ] in
           loop e |> Sresult.map ~f
         | _ -> assert false)
      (* paramter -> variant *)
      | _ when parp#exp exp -> failwith "not implemented!" (* TODO substitute variant *)
      | _ -> Expr (untyp_exp exp)
    in
    loop exp
  in
  let map_pat (Parsetree.{ ppat_desc; _ } as p) =
    let open Parsetree in
    match ppat_desc with
    | Ppat_var ident ->
      let txt = ident.txt ^ "_" ^ varp#name in
      Pat.var { ident with txt }
    | _ -> failwith "Var expected"
  in
  match str_item.str_desc with
  | Tstr_value (recf, [ vb ]) as source ->
    let pat = Untypeast.untype_pattern vb.vb_pat |> map_pat in
    let vb = Vb.mk pat @@ Sresult.get (spec_exp vb.vb_expr) in
    Str.value recf [ vb ]
  | Tstr_value _ -> assert false
  | _ -> failwith "Incorrect structure"
;;

let collect_info funp parp (s : Typedtree.structure_item) =
  let get_variants p =
    let get_type { c_lhs = { pat_type; pat_env; _ }; _ } = pat_type, pat_env in
    let rec loop count e =
      match e.exp_desc with
      | Texp_function ({ param; cases = [ ({ c_rhs; _ } as c) ]; _ } as d) ->
        if p param
        then (
          let ty, env = get_type c in
          let variants = Typespat.get_cons ty env in
          let parp = Predicate.par_of_ident param count in
          Some (parp, variants))
        else loop (count + 1) c_rhs
      | Texp_function _ -> assert false
      | _ -> None
    in
    loop 0
  in
  let collect funp parp str_item =
    let back d = { str_item with str_desc = d } in
    let ident_of_pattern = function
      | { pat_desc = Tpat_var (id, _); _ } -> id
      | _ -> failwith "Var expected."
    in
    match str_item.str_desc with
    (* one binding *)
    | Tstr_value (recf, [ vb ]) as source ->
      let fun_ident = ident_of_pattern vb.vb_pat in
      let funp = Predicate.fun_of_ident fun_ident in
      let cons = get_variants parp#ident vb.vb_expr in
      (match cons with
       | Some (parp, variants) -> funp, parp, variants
       | None -> failwith "Parameter not found.")
    | Tstr_value _ -> assert false
    | _ -> failwith "Not implemented"
  in
  collect funp parp s
;;

let parse_of_typed funp parp str_item =
  let struct_item_predicate funp str =
    match str.str_desc with
    | Tstr_value (_, vbl) as source -> List.exists (fun x -> funp#pat x.vb_pat) vbl
    | _ -> false
  in
  let untyped_str =
    let mapper = Untypeast.default_mapper in
    mapper.structure_item mapper
  in
  let p = struct_item_predicate funp in
  if p str_item
  then (
    let funp, parp, variants = collect_info funp parp str_item in
    let specs =
      List.map
        (fun variant ->
          let varp = Predicate.var_of_constr_desc variant variants in
          spec_str_item funp parp varp str_item)
        variants
    in
    untyped_str str_item :: specs)
  else untyped_str str_item |> fun x -> [ x ]
;;

let translate pp funp parp (t : Typedtree.structure) =
  let func = parse_of_typed funp parp in
  let str_items = List.map func t.str_items |> List.concat in
  Pprintast.structure Format.std_formatter str_items
;;
