open OCanren

let is_none x opt =
  conde
    [ x === !!true &&& (opt === !!None)
    ; x === !!false &&& Fresh.one (fun x' -> opt === !!(Some x'))
    ]
;;
