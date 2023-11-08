  $ cat ./le.src.ml
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
  $ SpecialKanren -ml -o a.out -par is -fname le le.src.ml
  $ cat a.out  
  open OCanren
  open OCanren.Std
  open Nat
  let rec le x y is =
    conde
      [(x === (!! O)) &&& (is === (!! true));
      ((x =/= (!! O)) &&& (y === (!! O))) &&& ((!! false) === is);
      Fresh.two (fun x' -> fun y' -> ((x === (!! (S x'))) &&& (y === (!! (S y')))) &&& (le x' y' is))]
  let rec le_false y x =
    ((x =/= (!! O)) &&& (y === (!! O))) ||| (Fresh.one (fun y' -> Fresh.one (fun x' -> (x === (!! (S (x')))) &&& ((y === (!! (S (y')))) &&& (le x' y' (!! false))))))
  let rec le_true y x = (x === (!! O)) ||| (Fresh.one (fun y' -> Fresh.one (fun x' -> (x === (!! (S (x')))) &&& ((y === (!! (S (y')))) &&& (le x' y' (!! true))))))
