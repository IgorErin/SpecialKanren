  $ cat src.ml
  open OCanren
  open OCanren.Std.Nat
  
  let is_zero num is =
    conde [ num === !!O &&& (is === !!true); num =/= !!O &&& (is === !!false) ]
  ;;
TODO  
$ SpecialKanren -ml -par num -fname is_zero src.ml -o a.out
$ ocamlformat --enable-outside-detected-project a.out 
