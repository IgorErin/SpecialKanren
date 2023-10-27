open OCanren

let is_none x opt =
  conde [ x === !!true &&& (opt === !!None); x === !!false &&& (opt === !!(Some 0)) ]
;;
