open Typedtree

exception Not_implemented

let vbs_of_tstr_opt (t : structure_item) =
  match t.str_desc with
  | Tstr_value (_, vbs) -> Some vbs
  | _ -> None
;;

let ident_of_vb vb =
  match vb.vb_pat.pat_desc with
  | Tpat_var (ident, _) -> ident
  | _ -> raise Not_implemented
;;

let parameter_check parp str =
  let rec loop count exp =
    let open Typedtree in
    match exp.exp_desc with
    | Texp_function { param; cases = [ { c_rhs; c_lhs; _ } ]; _ } ->
      if parp param
      then (
        let parp = Predicate.par_of_ident param count in
        let variants = Typespat.get_cons c_lhs.pat_type c_lhs.pat_env in
        Some (parp, variants))
      else loop (count + 1) c_rhs
    | Texp_function _ -> raise Not_implemented
    | _ -> None
  in
  loop 0 str
;;

let function_check funp parp (t : Typedtree.structure) =
  let ( >> ) f g x = f x |> g in
  t.str_items
  |> List.filter_map vbs_of_tstr_opt
  |> List.find_all @@ List.exists (ident_of_vb >> funp)
  |> function
  | [] -> raise Not_found
  | [ vbs ] ->
    (match vbs with
     | [] -> assert false
     | [ vb ] ->
       let ident = ident_of_vb vb in
       let funp = Predicate.fun_of_ident ident in
       parameter_check parp vb.vb_expr
       |> (function
       | Some (parp, variants) -> funp, parp, variants
       | None -> raise Not_found)
     | _ :: _ -> raise Not_implemented)
  | _ :: _ -> failwith "Found more than one"
;;
