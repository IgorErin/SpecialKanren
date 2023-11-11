open OCanren
open OCanren.Std
open Nat

let rec le x y is =
  conde
    [ x === !!O &&& (is === !!true)
    ; x =/= !!O &&& (y === !!O) &&& (!!false === is)
    ; Fresh.two (fun x' y' -> x === !!(S x') &&& (y === !!(S y')) &&& le x' y' is)
    ]
;;
