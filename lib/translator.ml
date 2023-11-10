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

type ('a, 'b) spec_fun =
  { source_info : Semant.Result.fun_info
  ; result : ('a, 'b) Semant.Result.t
  }

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

let step funs env (Semant.Result.{ fname; consts; _ } as source_info) =
  let { rglobals; rbody; _ } = find funs fname in
  let info = info_of_consts env rglobals consts in
  let result = Semant.run info rglobals rbody in
  { source_info; result }
;;

let run deps env funs =
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
  let set_call_self { source_info; _ } =
    match source_info.ref with
    | Some r ->
      let name = create_name source_info in
      let values = source_info.args in
      r := Some (name, values)
    | None -> ()
  in
  let set_external r { source_info; _ } =
    let name = create_name source_info in
    let values = source_info.args in
    r := Some (name, values)
  in
  let rec loop acc deps =
    (* let deps = set_already deps acc in *)
    let front = List.map (step funs env) deps in
    let new_front_deps =
      front
      |> List.concat_map (fun { result = { fun_infos; _ }; _ } -> fun_infos)
      |> List.filter_map (fun info ->
        let impl =
          List.find_opt (fun item -> Semant.Result.equal info item.source_info) acc
        in
        match impl, info.ref with
        | Some impl, Some r ->
          set_external r impl;
          None
        | _ -> Some info)
    in
    match new_front_deps with
    | [] -> front @ acc
    | _ :: _ -> loop (acc @ front) new_front_deps
  in
  let result = loop [] deps in
  result |> List.iter set_call_self;
  result
;;

let run funp parp variants str =
  let funs = get_funs str in
  let deps =
    variants
    |> List.map (fun var ->
      Semant.Result.
        { ref = None; fname = funp#ident; consts = [ parp#number, var ]; args = [] })
  in
  let result = run deps str.str_final_env funs in
  result
  |> List.iter (fun { result; _ } ->
    Semant.Result.pp Format.std_formatter result.dnf;
    Format.printf "\n")
;;

let translate funp parp (t : Typedtree.structure) =
  let _, funp, parp, variants = Validate.function_check funp#ident parp#by_ident t in
  let _ = run funp parp variants t in
  ()
;;
(* t.str_items |> List.map (Frontend.map_typed_item funp parp variants) |> List.concat *)
