let par_of_string name =
  object
    method ident x = String.equal name @@ Ident.name x
  end
;;

(* full path for now *)
let var_of_string path =
  let open Patterns in
  let open Ocanren_patterns in
  let spec_variant =
    path
    |> String.split_on_char '.'
    |> List.map Patterns.Gen.str
    |> fun x -> exp_by_texp_ident x
  in
  object
    method exp = parse_bool spec_variant
  end
;;

let fun_of_string name =
  let open Patterns in
  let open Gen in
  object
    method ident x = String.equal name @@ Ident.name x

    method pat =
      let id = Patterns.ident name in
      let pat_desc = Pattern_desc.tpat_var id drop in
      let pat_data = Patterns.pattern_data pat_desc drop drop drop drop drop in
      parse_bool pat_data
  end
;;

let par_of_ident id =
  object
    method ident x = Ident.equal id x

    method exp e =
      let open Patterns in
      let open Ocanren_patterns in
      let open Gen in
      let path = Path.pident in
      let path_pattern = Path.pident id in
      let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
      let p = expression exp_desc drop drop drop drop drop in
      parse_bool p e
  end
;;

let fun_of_ident id =
  let open Typedtree in
  let open Ocanren_patterns in
  object
    method ident x = Ident.equal id x

    method exp e =
      let open Patterns in
      let open Ocanren_patterns in
      let open Gen in
      let path = Path.pident in
      let path_pattern = Path.pident id in
      let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
      let p = expression exp_desc drop drop drop drop drop in
      parse_bool p e
  end
;;
