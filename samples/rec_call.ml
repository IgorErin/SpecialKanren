open OCanren
open OCanren.Std

let rec first x is =
  conde [ x === Nat.o &&& (is === !!false); Fresh.one (fun x -> first x !!true) ]
;;
