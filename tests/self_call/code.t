  $ cat src.ml
  open OCanren
  open OCanren.Std.List
  
  let rec self is = conde [ self is; is === !!false ]
  
  let rec self1 is =
    conde [ self1 !!false &&& (is === !!true); self1 !!true &&& (is === !!false) ]
  ;;
  $ SpecialKanren -ml -par is -fname self src.ml -o a.out
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std.List
  
  let rec self is = conde [ self is; is === !!false ]
  
  let rec self1 is =
    conde [ self1 !!false &&& (is === !!true); self1 !!true &&& (is === !!false) ]
  
  let rec self_false = self_false
  and self_true = self_true

  $ SpecialKanren -ml -par is -fname self1 src.ml -o a.out
  $ ocamlformat --enable-outside-detected-project a.out
  open OCanren
  open OCanren.Std.List
  
  let rec self is = conde [ self is; is === !!false ]
  
  let rec self1 is =
    conde [ self1 !!false &&& (is === !!true); self1 !!true &&& (is === !!false) ]
  
  let rec self1_false = self1_true
  and self1_true = self1_false

