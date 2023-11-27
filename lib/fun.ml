type 'a t =
  { name : Ident.t
  ; params : Ident.t list
  ; body : 'a
  }
