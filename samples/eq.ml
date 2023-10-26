open OCanren

let some_fun x = x

let spec_fun x y =
  conde [ x === !!true &&& (y === !!true); x === !!false &&& (y === !!false) ]
;;
