open OCanren

let id x y = x === y

let is_none opt is =
  conde
    [ Fresh.one (fun x ->
        conde
          [ Fresh.one (fun y -> opt === !!(Some y) &&& (is === !!true))
          ; opt === !!None &&& (is === !!false)
          ])
    ]
;;
