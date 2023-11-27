open Typedtree

type t = { funs : Canren.canren Fun.t list }

let get_funs { funs } = funs

let of_str str =
  str.str_items
  |> List.filter_map (fun str ->
    str.str_desc
    |> function
    | Tstr_value (_, vb) -> Some vb
    | _ -> None)
  |> List.concat
  |> List.filter_map (fun vb ->
    match vb.vb_pat.pat_desc with
    | Tpat_var (name, _) ->
      let params, canren = Canren.of_tast vb.vb_expr in
      let body =
        canren |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Canren fail")
      in
      Some Fun.{ params; name; body }
    | _ -> None)
  |> fun funs -> { funs }
;;

let find ~src ~name:src_name =
  src
  |> get_funs
  |> List.find_opt (fun Fun.{ name; _ } -> Ident.same src_name name)
  |> Core.Option.value_or_thunk ~default:(fun () ->
    Sexn.outer @@ Printf.sprintf "not found: %s" (Ident.name src_name))
;;
