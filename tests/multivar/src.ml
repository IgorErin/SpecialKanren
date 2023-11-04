open OCanren
open OCanren.Std

let rel opt bl =
  conde
    [ Fresh.one (fun x ->
        x
        === opt
        &&& conde
              [ Fresh.two (fun f s ->
                  opt === !!(Some f) &&& (bl === f) &&& (opt === !!(Some s)))
              ; Fresh.two (fun m s ->
                  opt === !!(Some m) &&& (bl === m) &&& (opt === !!(Some s)))
              ; bl === !!false &&& (opt === !!None)
              ])
    ]
;;
