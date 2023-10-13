open OCanren
open OCanren.Std
open Nat

let rec le x y is =
  conde
    [ x === o &&& (Bool.truo === is)
    ; x =/= o &&& (y === o) &&& (Bool.falso === is)
    ; Fresh.two (fun x' y' -> x === succ x' &&& (y === succ y') &&& le x' y' is)
    ]
;;

