let skip_ilogic expt =
  let open Types in
  let open Ocanren_patterns in
  match get_desc expt with
  | Tconstr (path, [ x ], _) when is_ilogic path -> x
  | Tconstr (path, _, _) ->
    Sexn.typespat @@ Printf.sprintf "Expected ilogic, path:%s" @@ Path.name path
  | _ -> Sexn.typespat "Seems like you want to specialize a ilogic parameter"
;;

let path_of_constr texp =
  let open Types in
  let desc = get_desc texp in
  match desc with
  | Tconstr (path, _, _) -> path
  | _ -> Sexn.typespat "Constructor expected."
;;

let look env path = Env.find_type path env

let get_manifest Types.{ type_manifest; type_loc; _ } =
  match type_manifest with
  | Some x -> x
  | None -> Location.raise_errorf ~loc:type_loc "Empty manifest"
;;

let get_variants Types.{ type_kind; type_loc; _ } =
  let open Types in
  match type_kind with
  | Type_variant (cons, _) -> cons
  | Type_abstract ->
    Location.raise_errorf ~loc:type_loc "Abstracted type. Expected variant."
  | Type_record _ -> Location.raise_errorf ~loc:type_loc "Record type. Expected variant."
  | Type_open -> Location.raise_errorf ~loc:type_loc "Open type. Expected variant."
;;

let desc_of_exp env type_exp =
  let open Types in
  type_exp
  |> path_of_constr
  |> look env
  |> get_variants
  |> List.map (fun { cd_id; _ } -> Env.find_ident_constructor cd_id env)
;;

let get_cons type_exp env = skip_ilogic type_exp |> desc_of_exp env
