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
  let open Typedtree in
  let funs = Outer.of_str str in
  let get x = Outer.find ~src:funs ~name:x in
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
