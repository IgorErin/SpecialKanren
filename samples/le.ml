open OCanren
open OCanren.Std
open Nat

let rec le x y is =
  conde
    [ x === o &&& (is === !!true)
    ; x =/= o &&& (y === o) &&& (!!false === is)
    ; Fresh.two (fun x' y' -> x === succ x' &&& (y === succ y') &&& le x' y' is)
    ]
;;
