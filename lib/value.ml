type const = Types.constructor_description * value list

and value =
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

let partition : value -> (Ident.t, const) Core.Either.t =
  let open Core in
  function
  | Var v -> Either.First v
  | Constr (desc, values) -> Either.Second (desc, values)
;;

let constr_get_opt = function
  | Constr (desc, values) -> Some (desc, values)
  | _ -> None
;;
