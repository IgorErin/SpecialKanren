let run funp parp variants str =
  let module P = Predicate in
  let src = Outer.of_str str in
  let name = P.Fun.Id.ident funp in
  let func = Outer.find ~src ~name in
  let targets =
    variants
    |> List.map (fun var ->
      let var = P.Var.create ~cur:var in
      let spec = [ Spec.{ var; par = parp } ] in
      Fun.{ spec; func })
  in
  Global.run ~src ~targets
;;

let translate funp parp (t : Typedtree.structure) =
  let _, funp, parp, variants =
    let fun_by_ident = Predicate.Fun.Str.by_ident funp in
    let par_by_ident = Predicate.Par.Str.by_ident parp in
    Validate.function_check fun_by_ident par_by_ident t
  in
  let result = run funp parp variants t |> List.map (fun Fun.{ func; _ } -> func) in
  let pstr = Untypeast.untype_structure t in
  let str = Dnf_past.run result in
  pstr @ [ str ]
;;
