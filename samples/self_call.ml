open OCanren
open OCanren.Std.List

let rec self is = conde [ self is; is === !!false ]

let rec self1 is =
  conde [ self1 !!false &&& (is === !!true); self1 !!true &&& (is === !!false) ]
;;
