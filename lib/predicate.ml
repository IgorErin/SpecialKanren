module Helpers = struct
  let exp_by_ident id e =
    let open Patterns in
    let open Gen in
    let path_pattern = PathPat.pident id in
    let exp_desc = Expression_desc.texp_ident path_pattern drop drop in
    let p = expression exp_desc drop drop drop drop drop in
    parse_bool p e
  ;;
end

let par_of_string name =
  object
    method ident x = String.equal name @@ Ident.name x
  end
;;

let var_of_constr_desc desc all =
  let open Typedtree in
  let open Types in
  let eq { cstr_name = fst; _ } { cstr_name = snd; _ } = String.equal fst snd in
  let get_cons e =
    match e.exp_desc with
    | Texp_construct (_, cd, _) -> Some cd
    | _ -> None
  in
  let texp_apply e =
    match e.exp_desc with
    | Texp_apply (hd, [ (_, Some texp) ]) when Ocanren_patterns.is_inj hd -> Some texp
    | _ -> None
  in
  let is_spec d = if eq desc d then Some () else None in
  let is_other d =
    let not_spec = not @@ eq d desc in
    let in_all = List.exists (fun x -> eq d x) all in
    if in_all && not_spec then Some () else None
  in
  let ( >>= ) = Option.bind in
  object
    method this e = texp_apply e >>= get_cons >>= is_spec |> Option.is_some
    method another e = texp_apply e >>= get_cons >>= is_other |> Option.is_some
    method name = desc.cstr_name

    method instance =
      let open Ast_helper in
      assert (List.length desc.cstr_args = 0);
      let lid = Longident.Lident desc.cstr_name in
      let ident = Exp.construct (Location.mkloc lid Location.none) None in
      ident
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

let par_of_ident id number =
  object
    method ident x = Ident.same id x
    method exp e = Helpers.exp_by_ident id e
    method number = number
  end
;;

let fun_of_ident id =
  object
    method ident x = Ident.same id x
    method exp e = Helpers.exp_by_ident id e
  end
;;
