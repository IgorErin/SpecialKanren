  $ cat ./src.ml
  open OCanren
  open OCanren.Std
  open Nat
  
  let rec le x y is =
    conde
      [ x === !!O &&& (is === !!true)
      ; x =/= !!O &&& (y === !!O) &&& (!!false === is)
      ; Fresh.two (fun x' y' -> x === !!(S x') &&& (y === !!(S y')) &&& le x' y' is)
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par is -fname le src.ml
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std
  open Nat
  
  let rec le x y is =
    conde
      [
        x === !!O &&& (is === !!true);
        x =/= !!O &&& (y === !!O) &&& (!!false === is);
        Fresh.two (fun x' y' ->
            x === !!(S x') &&& (y === !!(S y')) &&& le x' y' is);
      ]
  
  let rec le_false x y =
    x =/= !!O &&& (y === !!O)
    ||| Fresh.two (fun x' y' ->
            x === !!(S x') &&& (y === !!(S y') &&& le_false x' y'))
  
  and le_true x y =
    x === !!O
    ||| Fresh.two (fun x' y' ->
            x === !!(S x') &&& (y === !!(S y') &&& le_true x' y'))
