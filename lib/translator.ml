open Patterns

(* TODO (desc -> path etc)*)
let open_module_pat path =
  let path = Patterns.Path.match' path in
  let mod_expr_desc = Patterns.Module_expr_desc.tmod_ident path Gen.drop in
  let open_exp = Patterns.module_expr mod_expr_desc Gen.drop Gen.drop Gen.drop Gen.drop in
  let open_infos =
    Patterns.open_infos open_exp Gen.drop Gen.drop Gen.drop Gen.drop Gen.drop
  in
  let open_dec = open_infos in
  let desc = Structure_item_desc.tstr_open open_dec in
  structure_item desc Gen.drop Gen.drop
;;

let exp_apply head args =
  let exp_desc = Expression_desc.texp_apply head args in
  Gen.(expression exp_desc drop drop drop drop drop)
;;

let exp_texp_ident path =
  let desc = Gen.(Expression_desc.texp_ident path drop drop) in
  Gen.(expression desc drop drop drop drop drop)
;;

let exp_by_desc desc = Gen.(expression desc drop drop drop drop drop)

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

let translate (t : Typedtree.structure) =
  let slist x = Gen.list x in
  let pat =
    open_module_pat
      Gen.(
        slist [ str "OCanren" ]
        <|> slist [ str "OCanren"; str "Std" ]
        <|> slist [ str "OCanren"; str "Std"; str "Nat" ])
  in
  let map x =
    parse
      pat
      x
      (fun _ -> Printf.printf "win \n %!")
      (fun m ->
        Printf.printf "%s\n %!" m;
        Env.empty)
  in
  let _ = List.map map t.str_items in
  ()
;;
