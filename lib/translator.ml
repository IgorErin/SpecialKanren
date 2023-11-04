open Typedtree
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
  let new_name = funp#name ^ "_" ^ varp#name in
  match str_item.str_desc with
  | Tstr_value (recf, [ vb ]) ->
    let pat = Untypeast.untype_pattern vb.vb_pat |> var_with_name new_name in
    Semant.process parp varp vb.vb_expr |> fun x -> Str.value recf [ Vb.mk pat x ]
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
