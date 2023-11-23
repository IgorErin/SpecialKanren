let info_of_consts globals consts =
  consts
  |> List.map (fun (number, (const_desc : Types.constructor_description)) ->
    List.nth_opt globals number
    |> Core.Option.value_or_thunk ~default:(fun () -> Sexn.outer "Type_mismatch")
    |> fun ident ->
    let parp = Predicate.Par.Id.of_ident ~id:ident ~n:number in
    let varp = Predicate.Var.create ~cur:const_desc in
    parp, varp)
;;

let run funp parp variants str =
  let funs = Outer.of_str str in
  let get x = Outer.find ~src:funs ~name:x in
  let deps =
    variants
    |> List.map (fun var ->
      Global.
        { fname = Predicate.Fun.Id.ident funp
        ; consts = [ Predicate.Par.Id.number parp, var ]
        })
  in
  Global.run deps get info_of_consts
;;

let translate funp parp (t : Typedtree.structure) =
  let _, funp, parp, variants =
    let fun_by_ident = Predicate.Fun.Str.by_ident funp in
    let par_by_ident = Predicate.Par.Str.by_ident parp in
    Validate.function_check fun_by_ident par_by_ident t
  in
  let result = run funp parp variants t in
  let pstr = Untypeast.untype_structure t in
  let str = Dnf_past.run result in
  pstr @ [ str ]
;;
