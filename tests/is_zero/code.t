  $ cat src.ml
  open OCanren
  open OCanren.Std.Nat
  
  let is_zero num is =
    conde [ num === !!O &&& (is === !!true); num =/= !!O &&& (is === !!false) ]
  ;;
  $ SpecialKanren -ml -par is -fname is_zero src.ml -o a.out
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std.Nat
  
  let is_zero num is =
    conde [ num === !!O &&& (is === !!true); num =/= !!O &&& (is === !!false) ]
  
  let rec is_zero_false num = num =/= !!O
  and is_zero_true num = num === !!O
