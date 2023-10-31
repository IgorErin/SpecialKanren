open OCanren

let value opt default result =
  conde
    [ opt === !!None &&& (result === default)
    ; Fresh.one (fun x -> opt === !!(Some x) &&& (result === x))
    ]
;;
