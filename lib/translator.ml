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

let conde_pat = ()
let conj_pat = ()
let translate (t: Typedtree.structure) = 
  let slist = fun x -> Gen.list x String.equal in  
  let pat = open_module_pat Gen.(slist ["OCanren"] <|> slist ["OCanren"; "Std"] <|> slist ["OCanren"; "Std"; "Nat"]) in 
let map = fun x ->  parse pat x (fun _ -> Printf.printf "win \n %!") (fun m -> Printf.printf "%s\n %!" m; Env.empty) in
  let _ = List.map map t.str_items in ()
