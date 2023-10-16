open Patterns

let exp_by_texp_ident ident_list =
  let open Patterns in
  let open Gen in
  let path = list ident_list in
  let path_pattern = Path.match' path in
  let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
  expression exp_desc drop drop drop drop drop
;;

let exp_by_constr_ident name =
  let open Patterns in
  let open Gen in
  let type_desc = Patterns.Types.constructor_description name in
  let exp_desc = Expression_desc.texp_construct drop type_desc drop in
  expression exp_desc drop drop drop drop drop
;;

let exp_by_texp_apply hd arg_list =
  let open Gen in
  let exp_desc = Expression_desc.texp_apply hd arg_list in
  expression exp_desc drop drop drop drop drop
;;

let unify = Gen.(exp_by_texp_ident [ str "OCanren"; str "===" ])
let nunify = Gen.(exp_by_texp_ident [ str "OCanren"; str "=/=" ])
let conj = Gen.(exp_by_texp_ident [ str "OCanren"; str "&&&" ])
let disj = Gen.(exp_by_texp_ident [ str "OCanren"; str "|||" ])
let is_conde = parse_bool (exp_by_texp_ident Gen.[ str "OCanren"; str "conde" ])
let is_list_cons = parse_bool Gen.(exp_by_constr_ident @@ str "::")
let is_conj = parse_bool conj
let is_disj = parse_bool disj
let is_unify = parse_bool unify
let is_nunify = parse_bool nunify


