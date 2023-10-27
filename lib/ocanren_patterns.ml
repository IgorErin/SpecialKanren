open Patterns

let exp_texp_of_path ls =
  let open Patterns in
  let open Gen in
  let path = list ls in
  let path_pattern = PathPat.match' path in
  let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
  expression exp_desc drop drop drop drop drop
;;

let exp_by_constr_ident name =
  let open Patterns in
  let open Gen in
  let type_desc = Patterns.TypesPats.constructor_description name in
  let exp_desc = Expression_desc.texp_construct drop type_desc drop in
  expression exp_desc drop drop drop drop drop
;;

let exp_by_texp_apply hd arg_list =
  let open Gen in
  let exp_desc = Expression_desc.texp_apply hd arg_list in
  expression exp_desc drop drop drop drop drop
;;

let unify = Gen.(exp_texp_of_path [ str "OCanren"; str "===" ])
let nunify = Gen.(exp_texp_of_path [ str "OCanren"; str "=/=" ])
let conj = Gen.(exp_texp_of_path [ str "OCanren"; str "&&&" ])
let disj = Gen.(exp_texp_of_path [ str "OCanren"; str "|||" ])
let list_cons = Gen.(exp_by_constr_ident @@ str "::") (* check type.*)
let ilogic = Gen.(PathPat.match' @@ list [ str "OCanren__"; str "Logic"; str "ilogic" ])
let inj = Gen.(exp_texp_of_path [ str "OCanren"; str "!!" ])

(* TODO fresh one, two etc *)
let fresh = Gen.(exp_texp_of_path [ str "OCanren"; str "Fresh"; drop ])
let is_conde = parse_bool (exp_texp_of_path Gen.[ str "OCanren"; str "conde" ])
let is_conj = parse_bool conj
let is_disj = parse_bool disj
let is_list_cons = parse_bool list_cons  
let is_unify = parse_bool unify
let is_nunify = parse_bool nunify
let is_fresh = parse_bool fresh
let is_ilogic = parse_bool ilogic
let is_inj = parse_bool inj
