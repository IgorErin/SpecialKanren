open Patterns
open Gen

(* TODO (desc -> path etc)*)
let open_module_pat path =
  let path = Patterns.Path.match' path in
  let mod_expr_desc = Patterns.Module_expr_desc.tmod_ident path drop in
  let open_exp = Patterns.module_expr mod_expr_desc drop drop drop drop in
  let open_infos = Patterns.open_infos open_exp drop drop drop drop drop in
  let open_dec = open_infos in
  let desc = Structure_item_desc.tstr_open open_dec in
  structure_item desc drop drop
;;

(* let exp_by_desc desc = Gen.(expression desc drop drop drop drop drop)

let exp_apply head args =
  let exp_desc = Expression_desc.texp_apply head args in
  exp_by_desc exp_desc
;;

let exp_texp_ident path =
  let desc = Gen.(Expression_desc.texp_ident path drop drop) in
  Gen.(expression desc drop drop drop drop drop)
;;

module OCanren = struct
  let bin_op path left right =
    let arg path =
      Gen.pair Patterns.Arg_lable.noLable (Gen.some @@ exp_texp_ident path)
    in
    let head =
      let path = Path.match' path in
      Gen.(Expression_desc.texp_ident path drop drop)
    in
    exp_apply (exp_by_desc head) (Gen.list [ arg left; arg right ])
  ;;

  let conj =
    let path = Gen.(list [ str "OCanren"; str "&&&" ]) in
    bin_op path
  ;;

  let unify =
    let path = Gen.(list [ str "OCanren"; str "===" ]) in
    bin_op path
  ;;

  let non_unify =
    let path = Gen.(list [ str "OCanren"; str "=/=" ]) in
    bin_op path
  ;;
end

let strucute_item_value_nonrec vbs = 
  let open Gen in
  let expr_desc = Structure_item_desc.tstr_value Rec_flag.nonrecursive vbs in
  structure_item expr_desc drop drop

let exp_texp_function = 
  Expression_desc.texp_function

let func ident exp =
  let open Gen in
  let pattern_desc = Pattern_desc.tpat_var ident drop in
  let pattern = pattern_data pattern_desc drop drop drop drop drop in
  let vb = value_binding pattern exp drop drop in
  let vbs = Gen.list [ vb ] in
  strucute_item_value_nonrec vbs 
;;

let ident = Pat (fun x k -> 
  let name = Ident.name x in 
  Printf.printf "name: %s %!" name;
  k) *)

let translate (t : Typedtree.structure) =
  let open Gen in
  let pat = open_module_pat var in
  let map x =
    parse pat x (fun x -> Printf.printf "open %s\n" @@ String.concat "." x)
  in
  let x = List.map map t.str_items  in ()
  
;;
