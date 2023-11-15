open Typedtree

type ('a, 'b) raw_fun =
  { out_name : Ident.t
  ; out_globals : Ident.t list
  ; out_body : ('a, 'b) Canren.canren
  }

let get_funs str =
  str.str_items
  |> List.filter_map (fun str ->
    str.str_desc
    |> function
    | Tstr_value (_, vb) -> Some vb
    | _ -> None)
  |> List.concat
  |> List.filter_map (fun vb ->
    match vb.vb_pat.pat_desc with
    | Tpat_var (out_name, _) ->
      let out_globals, canren = Canren.of_tast vb.vb_expr in
      let out_body =
        canren |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Canren fail")
      in
      Some { out_globals; out_name; out_body }
    | _ -> None)
;;

let find funs ident =
  funs
  |> List.find_opt (fun { out_name; _ } -> Ident.same ident out_name)
  |> Core.Option.value_or_thunk ~default:(fun () ->
    Sexn.outer @@ Printf.sprintf "not found: %s" (Ident.name ident))
  |> fun { out_globals; out_body; _ } -> out_globals, out_body
;;

let info_of_consts env globals consts =
  consts
  |> List.map (fun (number, (const_desc : Types.constructor_description)) ->
    List.nth_opt globals number
    |> Core.Option.value_or_thunk ~default:(fun () -> Sexn.outer "Type_mismatch")
    |> fun ident ->
    let parp = Predicate.par_of_ident ident number in
    let varp =
      let type_constr = Typespat.desc_of_exp env const_desc.cstr_res in
      Predicate.var_of_constr_desc const_desc type_constr
    in
    parp, varp)
;;

let run funp parp variants str =
  let funs = get_funs str in
  let get x = find funs x in
  let create_info = info_of_consts str.str_final_env in
  let deps =
    variants
    |> List.map (fun var -> Global.{ fname = funp#ident; consts = [ parp#number, var ] })
  in
  Global.run deps get create_info
;;

let translate funp parp (t : Typedtree.structure) =
  let _, funp, parp, variants = Validate.function_check funp#ident parp#by_ident t in
  let result = run funp parp variants t in
  let pstr = Untypeast.untype_structure t in
  let str = Dnf_past.run result in
  pstr @ [ str ]
;;
