open Dnf
open Value
open Parsetree
open Ast_helper

let loc = Location.none
let create_pat ident = Pat.var @@ Location.mknoloc @@ Ident.name ident

(* copy past from noCanren *)
let create_apply f = function
  | [] -> f
  | args ->
    let args = List.map (fun a -> Asttypes.Nolabel, a) args in
    (match f.pexp_desc with
     | Pexp_apply (g, args') -> Exp.apply g (args' @ args)
     | _ -> Exp.apply f args)
;;

let create_apply_to_list f arg_list =
  let loc = f.pexp_loc in
  let new_arg =
    List.fold_right (fun x acc -> [%expr [%e x] :: [%e acc]]) arg_list [%expr []]
  in
  create_apply f [ new_arg ]
;;

let create_conj left right = [%expr [%e left] &&& [%e right]]

let create_disj = function
  | [] -> None
  | [ x ] -> Some x
  | [ x; y ] -> Some [%expr [%e x] ||| [%e y]]
  | l -> Some (create_apply_to_list [%expr conde] l)
;;

let create_fun var body = [%expr fun [%p create_pat var] -> [%e body]]

let create_fun_closer vars body =
  List.fold_right (fun var acc -> create_fun var acc) vars body
;;

let create_fresh var body = create_apply [%expr Fresh.one] [ create_fun var body ]
let create_inj expr = [%expr !![%e expr]]

(* end of copy past *)

let create_unify left right = [%expr [%e left] === [%e right]]
let create_disunify left right = [%expr [%e left] =/= [%e right]]

open Ast_helper

let lid_of_ident ident =
  ident
  |> Ident.name
  |> fun x -> Longident.Lident x |> fun x -> Location.mkloc x Location.none
;;

let lid_of_string str = Longident.Lident str |> fun x -> Location.mkloc x Location.none

let exp_of_value value =
  let rec create_const desc values =
    let exps = List.map inside_constr values in
    (match exps with
     | [] -> None
     | _ :: _ -> Some (Exp.tuple exps))
    |> fun arg ->
    let lid = desc.Types.cstr_name |> lid_of_string in
    Exp.construct lid arg
  and inside_constr = function
    | Var ident -> lid_of_ident ident |> Exp.ident
    | Constr (desc, values) -> create_const desc values
  and loop value =
    match value with
    | Var ident -> lid_of_ident ident |> Exp.ident
    | Constr (desc, values) -> create_const desc values |> create_inj
  in
  loop value
;;

let fresh_closer vars cont =
  List.fold_right (fun var acc -> create_fresh var acc) vars cont
;;

let exp_of_ident ident = lid_of_ident ident |> Exp.ident
let reduced = [%expr failwith "Reduced"]

let past_of_conj conj =
  let to_past_unify ident value =
    let left = exp_of_ident ident in
    let right = exp_of_value value in
    create_unify left right
  in
  let to_past_disunify ident value =
    let left = exp_of_ident ident in
    let right = exp_of_value value in
    create_disunify left right
  in
  let to_past_call ident values =
    let values = List.map exp_of_value values in
    let lident =
      Untypeast.lident_of_path ident |> fun x -> Location.mkloc x Location.none
    in
    let ident = Exp.ident lident in
    create_apply ident values
  in
  let to_past_one = function
    | DUnify (ident, value) -> to_past_unify ident value
    | DDisunify (ident, value) -> to_past_disunify ident value
    | DCall (ident, values) -> to_past_call ident values
    | DFresh _ -> failwith "DFresh unexpected here (TODO)"
  in
  (* copy past but compiler aware *)
  let to_past_regular current next =
    match current with
    | DUnify (ident, value) ->
      let current = to_past_unify ident value in
      create_conj current next
    | DDisunify (ident, value) ->
      let current = to_past_disunify ident value in
      create_conj current next
    | DCall (ident, values) ->
      let current = to_past_call ident values in
      create_conj current next
    | DFresh vars -> fresh_closer vars next
  in
  match List.rev conj with
  | [] -> reduced
  | hd :: tl ->
    let hd = to_past_one hd in
    List.fold_right to_past_regular (List.rev tl) hd
;;

let past_of_dnf dnf =
  List.map past_of_conj dnf |> create_disj |> Core.Option.value ~default:reduced
;;

let create_var source_info =
  let open Names_resolve in
  let postfix =
    source_info.consts
    |> List.map (fun (_, (x : Types.constructor_description)) -> x.cstr_name)
    |> String.concat "_"
  in
  Ident.name source_info.fname ^ "_" ^ postfix |> Location.mknoloc |> Ast_helper.Pat.var
;;

let create_vb Names_resolve.{ res_info; res_globals; res_dnf; _ } =
  let dnf = Names_resolve.to_dnf res_dnf in
  let body = past_of_dnf dnf in
  let f = create_fun_closer res_globals body in
  Vb.mk (create_var res_info) f
;;

let run ls = List.map create_vb ls |> Ast_helper.Str.value Recursive
