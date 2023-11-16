open OCanren
open OCanren.Std
open OCanren.Std.Nat

let twin fst snd thd x =
  x
  === !!true
  &&& conde
        [ fst === !!false &&& (snd === !!false) &&& (thd === !!false)
        ; fst === !!false &&& (snd === !!false) &&& (thd === !!true)
        ; fst === !!false &&& (snd === !!true) &&& (thd === !!false)
        ; fst === !!false &&& (snd === !!true) &&& (thd === !!true)
        ; fst === !!true &&& (snd === !!false) &&& (thd === !!false)
        ; fst === !!true &&& (snd === !!false) &&& (thd === !!true)
        ; fst === !!true &&& (snd === !!true) &&& (thd === !!false)
        ; fst === !!true &&& (snd === !!true) &&& (thd === !!true)
        ]
;;

let twins x is =
  conde
    [ twin !!false !!false is x; twin !!false !!false is x; twin !!false !!false is x ]
;;
