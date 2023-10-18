open OCanren
open OCanren.Std

let some_fun x = x

let spec_fun x y =
  conde [ x === Bool.truo &&& (y === Bool.truo); x === Bool.falso &&& (y === Bool.falso) ]
;;
