open OCanren

let id x y = x === y

let is_none opt is =
  conde
    [ Fresh.one (fun x ->
        conde
          [ Fresh.one (fun y -> id is is &&& (opt === !!(Some y)) &&& (is === !!true))
          ; id is is &&& (opt === !!None) &&& (is === !!false)
          ])
    ]
;;
