type item =
  { par : Predicate.Par.Id.t
  ; var : Predicate.Var.t
  }

type info = item list

let equal_item fst snd =
  let module P = Predicate in
  P.Par.Id.equal fst.par snd.par && P.Var.equal fst.var snd.var
;;

let equal fst snd = List.length fst = List.length snd && List.for_all2 equal_item fst snd

let create ~ident ~var =
  let open Predicate in
  { par = Par.Id.of_ident ~id:ident; var = Var.create ~cur:var }
;;
