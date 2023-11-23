type t

type raw_fun =
  { out_name : Ident.t
  ; out_globals : Ident.t list
  ; out_body : Canren.canren
  }

val of_str : Typedtree.structure -> t

(* find fun. convert in to canren *)
val find : src:t -> name:Ident.t -> raw_fun
