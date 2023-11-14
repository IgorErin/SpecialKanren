type const = Types.constructor_description * t list

and t =
  | Var of Ident.t
  | Constr of const

let rec to_string = function
  | Var v -> Printf.sprintf "%s" @@ Ident.name v
  | Constr (desc, values) ->
    values
    |> List.map to_string
    |> String.concat " "
    |> Printf.sprintf "%s (%s)" desc.cstr_name
;;

let rec same left right =
  match left, right with
  | Var left, Var right -> Ident.same left right
  | Constr (ld, lvs), Constr (rd, rvs) ->
    Types.may_equal_constr ld rd
    && List.combine lvs rvs |> List.for_all (fun (f, s) -> same f s)
  | _ -> false
;;

let compare left right =
  let list x = x |> List.find_opt (( <> ) 0) |> Option.value ~default:0 in
  let rec value left right =
    match left, right with
    | Var fst, Var snd -> Ident.compare fst snd
    | Var _, Constr _ -> -1
    | Constr _, Var _ -> 1
    | Constr (fd, fvs), Constr (sd, svs) ->
      let d = String.compare fd.cstr_name sd.cstr_name in
      if d = 0 then List.map2 value fvs svs |> list else 0
  in
  value left right
;;

let is_var = function
  | Var _ -> true
  | _ -> false
;;

let is_constr = function
  | Constr _ -> true
  | _ -> false
;;

let rec is_ground = function
  | Var _ -> false
  | Constr (_, values) -> List.for_all is_ground values
;;

let var_get = function
  | Var v -> v
  | _ -> failwith "Not a Var"
;;

let constr_get = function
  | Constr (desc, values) -> desc, values
  | _ -> failwith "Not a Constr"
;;

let partition : t -> (Ident.t, const) Core.Either.t =
  let open Core in
  function
  | Var v -> Either.First v
  | Constr (desc, values) -> Either.Second (desc, values)
;;

let constr_get_opt = function
  | Constr (desc, values) -> Some (desc, values)
  | _ -> None
;;
