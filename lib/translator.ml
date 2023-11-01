open Typedtree
open Ocanren_patterns
open Sresult

let ident_of_string name =
  let open Ast_helper in
  let lid = Longident.Lident name in
  let loc = Location.mkloc lid Location.none in
  Exp.ident loc
;;

let var_with_name new_name Parsetree.{ ppat_desc; _ } =
  let open Ast_helper in
  let open Parsetree in
  match ppat_desc with
  | Ppat_var ident -> Pat.var { ident with txt = new_name }
  | _ -> failwith "Var expected"
;;

let exp_failwith =
  let loc = Location.none in
  [%expr failwith "Relation reduced"]
;;

let inj_instance p =
  let loc = Location.none in
  Expr [%expr !![%e p#instance]]
;;

exception Unexpected_structure
exception Not_implemeted of string

let spec_str_item funp parp varp str_item =
  let open Ast_helper in
  let open Typedtree in
  let var_variant f s =
    (parp#by_exp f && varp#this s) || (parp#by_exp s && varp#this f)
  in
  let var_another_variant f s =
    (parp#by_exp f && varp#another s) || (parp#by_exp s && varp#another f)
  in
  let new_name = funp#name ^ "_" ^ varp#name in
  let untyp_exp = Untypeast.untype_expression in
  let spec_exp exp =
    let rec loop exp : Parsetree.expression sresult =
      match exp.exp_desc with
      | Texp_function { param; cases = [ { c_rhs; _ } ]; _ } when parp#by_ident param ->
        loop c_rhs
      | Texp_function
          { arg_label; param = _; cases = [ { c_lhs; c_guard; c_rhs } ]; partial = _ } ->
        let open Untypeast in
        let c_lhs = untype_pattern c_lhs in
        let c_guard = Option.map untype_expression c_guard in
        let c_rhs = loop c_rhs in
        (* TODO what in another cases ??? *)
        (match c_rhs with
         | Expr c_rhs -> Exp.fun_ arg_label c_guard c_lhs c_rhs |> fun x -> Expr x
         | _ -> c_rhs)
      | Texp_function { arg_label = _; param = _; cases; partial = _ } ->
        List.map
          (fun { c_lhs; c_guard; c_rhs } ->
            let open Untypeast in
            let c_lhs = untype_pattern c_lhs in
            let c_guard = Option.map untype_expression c_guard in
            let c_rhs = Sresult.get @@ loop c_rhs in
            (* TODO get above *)
            (* seems like we should create wildcard case with assert false *)
            Exp.case c_lhs ?guard:c_guard c_rhs)
          cases
        |> Exp.function_
        |> Sresult.expr
        (* conde *)
      | Texp_apply (hd_exp, args) when is_conde hd_exp ->
        (match args with
         | [ (lbf, Some e) ] ->
           let hd_exp = Untypeast.untype_expression hd_exp in
           let f next = Exp.apply hd_exp [ lbf, next ] in
           let loc = Location.none in
           Sresult.map ~f @@ (loop e |> Sresult.with_default [%expr []])
         | _ -> assert false)
      (* (::) list cons. assume disj *)
      | Texp_construct (_, _, args) when is_list_cons exp ->
        let fst, snd = Assert.bin args in
        let fst = loop fst in
        let snd = loop snd in
        let f x y =
          let loc = Location.none in
          [%expr [%e x] :: [%e y]]
        in
        reduce_disj fst snd f
      (* (|||) disj *)
      | Texp_apply (hd_exp, args) when is_disj hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        let fexp = loop fexp in
        let sexp = loop sexp in
        let cons x y =
          let loc = Location.none in
          [%expr [%e x] ||| [%e y]]
        in
        reduce_disj fexp sexp cons
        (* (&&&) conj *)
      | Texp_apply (hd_exp, args) when is_conj hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        let fexp = loop fexp in
        let sexp = loop sexp in
        let cons x y =
          let loc = Location.none in
          [%expr [%e x] &&& [%e y]]
        in
        reduce_conj fexp sexp cons
      (* === *)
      | Texp_apply (hd_exp, args) when is_unify hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        (match () with
         | () when var_variant fexp sexp -> Empty
         | () when var_another_variant fexp sexp -> ReduceConj
         | _ -> Expr (untyp_exp exp))
      (* =/= *)
      | Texp_apply (hd_exp, args) when is_nunify hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        (match () with
         | () when var_variant fexp sexp -> ReduceConj
         | () when var_another_variant fexp sexp -> Empty
         | _ -> Expr (untyp_exp exp))
      (* rec call *)
      | Texp_apply (hd_exp, ls) when funp#exp hd_exp ->
        let _, arg = List.nth ls parp#number in
        (match arg with
         | Some arg ->
           let app_without_param =
             let new_args =
               ls
               |> List.filteri (fun i _ -> i <> parp#number)
               |> List.filter_map (fun (lb, vl) ->
                 Option.map (fun x -> lb, Sresult.get @@ loop x) vl)
             in
             let hd = ident_of_string new_name in
             Exp.apply hd new_args
           in
           (match () with
            | () when parp#by_exp arg || varp#this arg ->
              (* simply delete argument by number (is it correct???) *)
              Expr app_without_param
            | () when varp#another arg ->
              (* if another variant in place of spec par -> reduce conj *)
              ReduceConj
              (* if some_var -> add (some_var === spec_variant) in conj with apply *)
            | () ->
              let loc = Location.none in
              Expr
                [%expr
                  [%e app_without_param] &&& ([%e untyp_exp arg] === !!varp#instance)])
         | None -> raise @@ Not_implemeted "Abstracted over spec paramter.")
      | Texp_apply (hd, args) when is_fresh hd ->
        (* TODO if fresh var unif with variant *)
        let lb, e = Assert.lb_arg args in
        let f x = Exp.apply (untyp_exp hd) [ lb, x ] in
        loop e |> Sresult.map ~f
      | Texp_apply (hd, args) ->
        let hd = untyp_exp hd in
        let args =
          List.filter_map
            (fun (lb, x) ->
              Option.bind x (fun x ->
                loop x |> Sresult.map ~f:(fun x -> lb, x) |> Sresult.to_opt))
            args
        in
        Expr (Exp.apply hd args)
      (* paramter -> variant *)
      | _ when parp#by_exp exp -> inj_instance varp
      | _ -> Expr (untyp_exp exp)
    in
    loop exp
  in
  match str_item.str_desc with
  | Tstr_value (recf, [ vb ]) ->
    let pat = Untypeast.untype_pattern vb.vb_pat |> var_with_name new_name in
    let () = Semant.process parp varp vb.vb_expr in
    spec_exp vb.vb_expr
    |> Sresult.get_with_default exp_failwith
    |> fun x -> Str.value recf [ Vb.mk pat x ]
  | Tstr_value _ ->
    (* should be checked at validating *)
    assert false
  | _ -> raise Unexpected_structure
;;

module Frontend = struct
  let ident_of_vb_opt vb =
    match vb.vb_pat.pat_desc with
    | Tpat_var (ident, _) -> Some ident
    | _ -> None
  ;;

  let struct_item_predicate funp str =
    match str.str_desc with
    | Tstr_value (_, [ vb ]) ->
      ident_of_vb_opt vb
      |> (function
      | Some ident -> funp#ident ident
      | None -> false)
    | _ -> false
  ;;

  let map_typed_item funp parp variants str_item =
    let untyped_str_item =
      let mapper = Untypeast.default_mapper in
      mapper.structure_item mapper
    in
    let p = struct_item_predicate funp in
    if p str_item
    then (
      let specs =
        (* spec for each variant *)
        variants
        |> List.map (fun variant ->
          let varp = Predicate.var_of_constr_desc variant variants in
          spec_str_item funp parp varp str_item)
      in
      let source = untyped_str_item str_item in
      source :: specs)
    else untyped_str_item str_item |> fun x -> [ x ]
  ;;
end

let translate funp parp (t : Typedtree.structure) =
  let funp, parp, variants = Validate.function_check funp#ident parp#by_ident t in
  t.str_items |> List.map (Frontend.map_typed_item funp parp variants) |> List.concat
;;
