let run funp parp variants str =
  let src = Outer.of_str str in
  let tgt =
    variants
    |> List.map (fun var ->
      Global.
        { fname = Predicate.Fun.Id.ident funp
        ; consts = [ Predicate.Par.Id.number parp, var ]
        })
  in
  Global.run ~src ~tgt
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
