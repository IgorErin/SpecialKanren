open OCanren
open OCanren.Std
open OCanren.Std.Nat

let spec fst snd opt =
  conde
    [ fst === !!true &&& (opt === !!None) &&& (snd === !!true)
    ; fst === !!false &&& (snd === !!false) &&& Fresh.one (fun x -> opt === !!(Some x))
    ]
;;

let rel is =
  conde
    [ is === !!false &&& spec !!true !!true !!None
    ; Fresh.one (fun x -> spec !!false !!false !!(Some x)) &&& (is === !!false)
    ]
;;
