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
