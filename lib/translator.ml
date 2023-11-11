open Typedtree

type ('a, 'b) raw_fun =
  { rname : Ident.t
  ; rglobals : Ident.t list
  ; rbody : ('a, 'b) Canren.canren
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
    | Tpat_var (ident, _) ->
      let rglobals, canren = Canren.of_tast vb.vb_expr in
      let canren = canren |> Option.get in
      let rglobals = List.rev rglobals in
      Some { rname = ident; rglobals; rbody = canren }
    | _ -> None)
;;

let find funs path =
  funs
  |> List.find_opt (fun { rname; _ } -> Ident.same path rname)
  |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Not fun found in str")
;;

let info_of_consts env globals consts =
  consts
  |> List.map (fun (number, (const_desc : Types.constructor_description)) ->
    List.nth_opt globals number
    |> Core.Option.value_or_thunk ~default:(fun () -> failwith "arg number out of range")
    |> fun ident ->
    let parp = Predicate.par_of_ident ident number in
    let varp =
      let type_constr = Typespat.desc_of_exp env const_desc.cstr_res in
      Predicate.var_of_constr_desc const_desc type_constr
    in
    parp, varp)
;;

let step funs env Semant.Result.{ fname; consts } =
  let { rglobals; rbody; _ } = find funs fname in
  let info = info_of_consts env rglobals consts in
  Semant.run info fname rglobals rbody
;;

let resolve soruce env funs =
  let create_name source_info =
    let open Semant.Result in
    let postfix =
      source_info.consts
      |> List.map (fun (num, (x : Types.constructor_description)) ->
        Int.to_string num ^ x.cstr_name)
      |> String.concat "_"
    in
    Ident.name source_info.fname ^ "_" ^ postfix
  in
  let trans = step funs env in
  let set_call_self Semant.Result.{ href; hfinfo; hargs; _ } =
    href := Some (create_name hfinfo, hargs)
  in
  let filter acc deps =
    let open Semant.Result in
    deps
    |> List.filter_map (fun (hole : hole_info) ->
      List.find_opt (fun item -> Semant.Result.equal hole.hfinfo item.res_info) acc
      |> function
      | Some _ ->
        set_call_self hole;
        None
      | _ -> Some hole)
  in
  let rec loop acc deps =
    let open Semant.Result in
    let front = List.map (fun hole -> trans hole.hfinfo) deps in
    let deps =
      let deps = List.concat_map (fun res -> res.res_deps) front @ deps in
      filter acc deps
    in
    let acc = front @ acc in
    match deps with
    | [] -> acc
    | _ :: _ -> loop acc deps
  in
  let init = List.map trans soruce in
  let deps =
    init |> List.concat_map (fun Semant.Result.{ res_deps; _ } -> res_deps) |> filter init
  in
  loop init deps
;;

let run funp parp variants str =
  let funs = get_funs str in
  let deps =
    variants
    |> List.map (fun var ->
      Semant.Result.{ fname = funp#ident; consts = [ parp#number, var ] })
  in
  resolve deps str.str_final_env funs
;;

let translate funp parp (t : Typedtree.structure) =
  let _, funp, parp, variants = Validate.function_check funp#ident parp#by_ident t in
  let result = run funp parp variants t in
  let pstr = Untypeast.untype_structure t in
  let str = Dnf_past.run result in
  pstr @ [ str ]
;;
