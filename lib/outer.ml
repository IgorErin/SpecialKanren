open Typedtree

type raw_fun =
  { out_name : Ident.t
  ; out_globals : Ident.t list
  ; out_body : Canren.canren
  }

type t = { funs : raw_fun list }

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
    | Tpat_var (out_name, _) ->
      let out_globals, canren = Canren.of_tast vb.vb_expr in
      let out_body =
        canren |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Canren fail")
      in
      Some { out_globals; out_name; out_body }
    | _ -> None)
  |> fun funs -> { funs }
;;

let find ~src ~name =
  src
  |> get_funs
  |> List.find_opt (fun { out_name; _ } -> Ident.same name out_name)
  |> Core.Option.value_or_thunk ~default:(fun () ->
    Sexn.outer @@ Printf.sprintf "not found: %s" (Ident.name name))
;;
