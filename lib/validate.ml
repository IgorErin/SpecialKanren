open Typedtree

exception Not_implemented of string
exception Fun_not_found
exception Par_not_found
exception MT1_fun_found

let vbs_of_tstr_opt (t : structure_item) =
  match t.str_desc with
  | Tstr_value (_, vbs) -> Some vbs
  | _ -> None
;;

let ident_of_vb vb =
  match vb.vb_pat.pat_desc with
  | Tpat_var (ident, _) -> ident
  | _ -> raise @@ Not_implemented "Only var pattern in value binding supported"
;;

let parameter_check parp env str =
  let rec loop count exp =
    let open Typedtree in
    match exp.exp_desc with
    | Texp_function { param; cases = [ { c_rhs; c_lhs; _ } ]; _ } ->
      if parp param
      then (
        let parp = Predicate.par_of_ident param count in
        let variants = Typespat.get_cons c_lhs.pat_type env in
        Some (parp, variants))
      else loop (count + 1) c_rhs
    | Texp_function _ -> raise @@ Not_implemented "Only one case functions supported"
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
  | [] -> raise Fun_not_found
  | [ vbs ] ->
    (match vbs with
     | [] -> assert false
     | [ vb ] ->
       let ident = ident_of_vb vb in
       let funp = Predicate.fun_of_ident ident in
       parameter_check parp t.str_final_env vb.vb_expr
       |> (function
        | Some (parp, variants) -> vb.vb_expr, funp, parp, variants
        | None -> raise Par_not_found)
     | _ :: _ -> raise @@ Not_implemented "Only non mutually recursive funtions supported")
  | _ :: _ -> raise MT1_fun_found
;;
