open OCanren
open OCanren.Std

let false_and_true x y = conde [ (x =/= Bool.truo) &&& (y === Bool.falso); y === Bool.truo ]
