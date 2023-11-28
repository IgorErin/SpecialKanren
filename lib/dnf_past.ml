open Dnf
open Value
open Parsetree
open Ast_helper

module Prim = struct
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

  let fresh_closer vars body =
    let create_fresh var body = create_apply [%expr Fresh.one] [ create_fun var body ] in
    let fresh_closer vars cont =
      List.fold_right (fun var acc -> create_fresh var acc) vars cont
    in
    match vars with
    | [] -> create_apply [%expr Fresh.zero] [ body ]
    | [ var ] -> create_apply [%expr Fresh.one] [ create_fun var body ]
    | [ fst; snd ] ->
      create_apply [%expr Fresh.two] [ create_fun_closer [ fst; snd ] body ]
    | [ fst; snd; thd ] ->
      create_apply [%expr Fresh.three] [ create_fun_closer [ fst; snd; thd ] body ]
    | [ fst; snd; thd; fth ] ->
      create_apply [%expr Fresh.four] [ create_fun_closer [ fst; snd; thd; fth ] body ]
    | [ fst; snd; thd; fth; fvt ] ->
      create_apply
        [%expr Fresh.five]
        [ create_fun_closer [ fst; snd; thd; fth; fvt ] body ]
    | ls -> fresh_closer ls body
  ;;

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
      | Constr (desc, values) -> create_const desc values |> create_inj
    and loop value =
      match value with
      | Var ident -> lid_of_ident ident |> Exp.ident
      | Constr (desc, values) -> create_const desc values |> create_inj
    in
    loop value
  ;;

  let exp_of_ident ident = lid_of_ident ident |> Exp.ident
  let reduced = [%expr failwith "Reduced"]
end

let past_of_conj conj =
  let to_past_unify ident value =
    let left = Prim.exp_of_value ident in
    let right = Prim.exp_of_value value in
    Prim.create_unify left right
  in
  let to_past_disunify ident value =
    let left = Prim.exp_of_value ident in
    let right = Prim.exp_of_value value in
    Prim.create_disunify left right
  in
  let to_past_call ident values =
    let values = List.map Prim.exp_of_value values in
    let lident =
      Untypeast.lident_of_path ident |> fun x -> Location.mkloc x Location.none
    in
    let ident = Exp.ident lident in
    Prim.create_apply ident values
  in
  let to_past_one = function
    | DUnify (ident, value) -> to_past_unify ident value
    | DDisunify (ident, value) -> to_past_disunify ident value
    | DCall (ident, values) -> to_past_call ident values
    | DFresh _ -> Sexn.dnf_past "Fresh in last close"
  in
  (* copy past but compiler aware *)
  let to_past_regular current next =
    match current with
    | DUnify (ident, value) ->
      let current = to_past_unify ident value in
      Prim.create_conj current next
    | DDisunify (ident, value) ->
      let current = to_past_disunify ident value in
      Prim.create_conj current next
    | DCall (ident, values) ->
      let current = to_past_call ident values in
      Prim.create_conj current next
    | DFresh vars -> Prim.fresh_closer vars next
  in
  match List.rev conj with
  | [] -> failwith "Empty conj in ast generation"
  | hd :: tl ->
    let hd = to_past_one hd in
    List.fold_right to_past_regular (List.rev tl) hd
;;

let past_of_dnf dnf =
  dnf
  |> List.filter (fun x -> not @@ Core.List.is_empty x)
  |> List.map past_of_conj
  |> Prim.create_disj
  |> Core.Option.value ~default:Prim.reduced
;;

let create_var name = name |> Ident.name |> Location.mknoloc |> Ast_helper.Pat.var

let create_vb Fun.{ name; params; body } =
  let body = past_of_dnf body in
  let f = Prim.create_fun_closer params body in
  Vb.mk (create_var name) f
;;

let run ls = ls |> List.map create_vb |> Ast_helper.Str.value Recursive
