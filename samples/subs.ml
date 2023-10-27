open OCanren
open OCanren.Std.Nat

let rel x y is = conde [ x === o &&& (is === !!false); y === o ]

let myfun x y is =
  conde [ x === o &&& (is === !!true); y === succ o &&& (is === !!false); rel x y is ]
;;
