open OCanren
open OCanren.Std

let rec first x is =
  conde [ x === Nat.o &&& (is === Bool.falso); Fresh.one (fun x -> first x Bool.truo) ]
;;
