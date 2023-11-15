open OCanren
open OCanren.Std.Nat

let is_zero num is =
  conde [ num === !!O &&& (is === !!true); num =/= !!O &&& (is === !!false) ]
;;
