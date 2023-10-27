open Typedtree

type 'a sresult =
  | Expr of 'a
  | ReduceConj
  | Empty

let map ~f = function
  | Expr x -> Expr (f x)
  | Empty -> Empty
  | ReduceConj -> ReduceConj
;;

let bind ~f = function
  | Expr x -> f x
  | Empty -> Empty
  | ReduceConj -> ReduceConj
;;

let reduce_conj fst snd cons =
  match fst, snd with
  | Expr fst, Expr snd -> Expr (cons fst snd)
  | Empty, Expr x | Expr x, Empty -> Expr x
  | Empty, Empty -> Empty
  | _ -> ReduceConj
;;

let reduce_disj fst snd cons =
  match fst, snd with
  | Expr fst, Expr snd -> Expr (cons fst snd)
  | Empty, Expr x | Expr x, Empty | ReduceConj, Expr x | Expr x, ReduceConj -> Expr x
  | ReduceConj, Empty | Empty, ReduceConj | Empty, Empty -> Empty
  | ReduceConj, ReduceConj -> ReduceConj
;;

let get = function
  | Expr x -> x
  | _ -> failwith "Expr expected."
;;

let is_reduce_conj = function
  | ReduceConj -> true
  | _ -> false
;;

let with_default x = function
  | Expr x -> Expr x
  | _ -> Expr x
;;

let to_opt = function
  | Expr x -> Some x
  | _ -> None
;;
