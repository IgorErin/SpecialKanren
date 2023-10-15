(* TODO (desc -> path etc)*)

open Typedtree
open Asttypes
open Tast_mapper
open Ocanren_patterns
open Patterns

(*
   delete *)
let delete_atom _ _ = false

(* iterate over expression *)
let delete_conj _ = false

let is_conde expr =
  let open Patterns in
  let path = Gen.(list [ str "Ocanren"; str "conde" ]) in
  let path_pattern = Path.match' path in
  match expr.exp_desc with
  | Texp_ident (path, _, _) -> parse path_pattern path (fun _ -> None)
  | _ -> failwith "is_conde"
;;

(* TODO assert on type of cons *)
let is_list_cons _ = false

(* first: check that iterate inside conde *)

let is_predicate _ = false
let is_spec_param _ = false
let is_atom _ = false
let is_conj _ = false
let spec_atom _ = None
let is_spec _ = false

let pair cons fst snd =
  match fst, snd with
  | Some fst, Some snd ->
    let result = cons fst snd in
    Some result
  | None, Some snd -> Some snd
  | Some fst, None -> Some fst
  | _ -> None
;;

let rec spec_map : expression -> expression option =
  fun expr ->
  match expr.exp_desc with
  (* disj construction *)
  | Texp_construct (loc, desc, [ fst; snd ]) when is_list_cons desc ->
    let fst = spec_map fst in
    let snd = spec_map snd in
    let cons fst snd =
      { expr with exp_desc = Texp_construct (loc, desc, [ fst; snd ]) }
    in
    pair cons fst snd
  (* atom formulas consturction *)
  | Texp_apply (exp, [ (lbf, Some fst); (lbs, Some snd) ]) when is_conj exp ->
    let fst = spec_map fst in
    let snd = spec_map snd in
    let cons x y =
      { expr with exp_desc = Texp_apply (exp, [ lbf, Some x; lbs, Some y ]) }
    in
    pair cons fst snd
  | Texp_apply (exp, _) as e when is_predicate exp ->
    if is_spec e then None else Some { expr with exp_desc = e }
  | _ -> failwith "not implemented"
;;

let nil = failwith "nil"

let rec general_map expr =
  match expr.exp_desc with
  | Texp_apply (exp, [ (lb, Some arg) ]) when is_conde exp ->
    let new_arg =
      spec_map arg
      |> function
      | None -> nil
      | Some x -> [ lb, Some x ]
    in
    let new_desc = Texp_apply (exp, new_arg) in
    { expr with exp_desc = new_desc }
  | Texp_function
      ({ param; cases = ({ c_rhs; _ } as c) :: tl; partial = Total; _ } as desc) ->
    if is_spec_param param
    then general_map c_rhs
    else (
      (* get next expr and construct back *)
      let next_expr = general_map c_rhs in
      let new_case = { c with c_rhs = next_expr } in
      let new_cases = new_case :: tl in
      { expr with exp_desc = Texp_function { desc with cases = new_cases } })
  | _ -> failwith "General map. Not implemented"
;;

let mapper expr =
  match expr.exp_desc with
  | Texp_function
      { arg_label : arg_label
      ; param : Ident.t
      ; cases : value case list
      ; partial : partial
      } ->
    Printf.printf "case len: %d\n" @@ List.length cases;
    Printf.printf "ident: %s\n" @@ Ident.name param
  | _ -> Printf.printf "lol\n"
;;

let translate (t : Typedtree.structure) =
  let map =
    { Tast_iterator.default_iterator with
      expr =
        (fun self expr ->
          mapper expr;
          Tast_iterator.default_iterator.expr self expr)
    }
  in
  List.iter (map.structure_item map) t.str_items
;;
