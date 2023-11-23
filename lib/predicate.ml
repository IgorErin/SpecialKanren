module Helpers = struct
  let string_ident_equal str ident = String.equal str @@ Ident.name ident
end

module Par = struct
  module Str = struct
    type t = string

    let of_string x = x
    let by_ident = Helpers.string_ident_equal
  end

  module Id = struct
    type t =
      { ident : Ident.t
      ; number : int
      }

    let of_ident ~id ~n = { ident = id; number = n }
    let by_ident { ident; _ } = Ident.same ident
    let ident { ident; _ } = ident
    let number { number; _ } = number
  end
end

module Fun = struct
  module Str = struct
    type t = string

    let of_string x = x
    let by_ident = Helpers.string_ident_equal
  end

  module Id = struct
    type t = Ident.t

    let of_ident x = x
    let name = Ident.name
    let by_ident = Ident.same
    let ident x = x
  end
end

module Var = struct
  type const = Types.constructor_description
  type t = { current : const }

  let create ~cur = { current = cur }
  let desc { current; _ } = current
  let arity { current; _ } = current.cstr_arity
end

(* let par_of_string name =
  object
    method by_ident x = String.equal name @@ Ident.name x
  end
;;

let var_of_constr_desc desc all =
  let open Typedtree in
  let open Types in
  let eq fst snd = Types.equal_tag fst.cstr_tag snd.cstr_tag in
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
  let ( >> ) f g x = f x |> g in
  let open Core.Option.Monad_infix in
  object
    method this e = texp_apply e >>= get_cons >>= is_spec |> Option.is_some
    method another e = texp_apply e >>= get_cons >>= is_other |> Option.is_some
    method name = desc.cstr_name
    method cd_another = is_other >> Option.is_some
    method desc = desc
    method by_desc = eq desc
    method arity = desc.cstr_arity

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
      (parse_bool pat_data : Typedtree.pattern -> bool)
  end
;;

let par_of_ident id number =
  object
    method by_ident x = Ident.same id x
    method by_exp e = Helpers.exp_by_ident id e
    method number = number
    method ident = id
  end
;;

let fun_of_ident id =
  object
    method by_ident x = Ident.same id x
    method ident = id
    method exp e = Helpers.exp_by_ident id e
    method name = Ident.name id
  end
;; *)
