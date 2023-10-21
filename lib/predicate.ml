let par_of_string name =
  object
    method ident x = String.equal name @@ Ident.name x
  end
;;

(* full path for now *)
let var_of_string path =
  let open Patterns in
  let open Ocanren_patterns in
  let path_of_string ls =
    let rec helper acc = function
      | hd :: tl -> helper (Path.Pdot (acc, hd)) tl
      | [] -> acc
    in
    let ls = List.rev ls in
    let start = Path.Pident (Ident.create_persistent @@ List.hd ls) in
    helper start ls
  in
  let str_path = path |> String.split_on_char '.' in
  let path = path_of_string str_path in
  object
    method exp =
      str_path |> List.map Patterns.Gen.str |> fun x -> exp_by_texp_ident x |> parse_bool

    method map_exp exp = failwith "not implemented"
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

let exp_by_ident id e =
  let open Patterns in
  let open Ocanren_patterns in
  let open Gen in
  let path = PathPat.pident in
  let path_pattern = PathPat.pident id in
  let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
  let p = expression exp_desc drop drop drop drop drop in
  parse_bool p e
;;

let par_of_ident id =
  object
    method ident x = Ident.equal id x
    method exp e = exp_by_ident id e
  end
;;

let fun_of_ident id =
  object
    method ident x = Ident.equal id x
    method exp e = exp_by_ident id e
  end
;;
