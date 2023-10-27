let skip_ilogic expt =
  let open Types in
  let open Ocanren_patterns in
  match get_desc expt with
  | Tconstr (path, [ x ], _) when is_ilogic path -> x
  | _ -> failwith "ilogic expected"
;;

let path_of_constr texp =
  let open Types in
  match get_desc texp with
  | Tconstr (path, _, _) -> path
  | _ -> failwith "Tconstr expected."
;;

let look env path = Env.find_type path env

let get_manifest Types.{ type_manifest; _ } =
  match type_manifest with
  | Some x -> x
  | None -> failwith "Empty manifest"
;;

let get_variants Types.{ type_kind; _ } =
  let open Types in
  match type_kind with
  | Type_variant (cons, _) -> cons
  | Type_abstract -> failwith "abstract."
  | Type_record _ -> failwith "record."
  | Type_open -> failwith "open"
;;

let subst_ident id path =
  let open Path in
  let rec loop = function
    | Pident _ -> Pident id
    | Pdot (next, name) -> Pdot (loop next, name)
    | Papply _ -> failwith "Papply"
  in
  loop path
;;

let get_cons type_exp env =
  let open Types in
  skip_ilogic type_exp
  |> path_of_constr
  |> look env
  |> get_variants
  |> List.map (fun { cd_id; _ } -> Env.find_ident_constructor cd_id env)
;;
