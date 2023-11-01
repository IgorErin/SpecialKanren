open OCanren

let value opt default result =
  conde [ opt === !!None &&& (result === default); opt === !!(Some result) ]
;;
