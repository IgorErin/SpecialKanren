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
      ; num : int
      }

    let of_ident ~id ~num = { ident = id; num }
    let by_ident { ident; _ } = Ident.same ident
    let ident { ident; _ } = ident
    let equal fst snd = Ident.same fst.ident snd.ident
    let name { ident; _ } = Ident.name ident
    let num { num; _ } = num
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
  let equal fst snd = Types.equal_tag fst.current.cstr_tag snd.current.cstr_tag
  let name { current } = current.cstr_name
end
