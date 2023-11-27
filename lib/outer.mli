type t

val of_str : Typedtree.structure -> t

(* find fun. convert in to canren *)
val find : src:t -> name:Ident.t -> Canren.canren Fun.t
