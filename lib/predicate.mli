module Par : sig
  module Str : sig
    type t

    val of_string : string -> t
    val by_ident : t -> Ident.t -> bool
  end

  module Id : sig
    type t

    val of_ident : id:Ident.t -> t
    val by_ident : t -> Ident.t -> bool
    val ident : t -> Ident.t
    val equal : t -> t -> bool
    val name : t -> string
  end
end

module Fun : sig
  module Str : sig
    type t

    val of_string : string -> t
    val by_ident : t -> Ident.t -> bool
    (* val by_pat : Typedtree.pattern -> bool *)
  end

  module Id : sig
    type t

    val of_ident : Ident.t -> t
    val name : t -> string
    val by_ident : t -> Ident.t -> bool
    val ident : t -> Ident.t
  end
end

module Var : sig
  type t
  type const = Types.constructor_description

  val create : cur:const -> t
  val desc : t -> const
  val arity : t -> int
  val equal : t -> t -> bool
  val name : t -> string
end
