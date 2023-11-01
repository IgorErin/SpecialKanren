  $ cat ./le.src.ml
  open OCanren
  open OCanren.Std
  open Nat
  
  let rec le x y is =
    conde
      [ x === o &&& (is === !!true)
      ; x =/= o &&& (y === o) &&& (!!false === is)
      ; Fresh.two (fun x' y' -> x === succ x' &&& (y === succ y') &&& le x' y' is)
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par is -fname le le.src.ml
  $ cat a.out  
  open OCanren
  open OCanren.Std
  open Nat
  let rec le x y is =
    conde
      [(x === o) &&& (is === (!! true));
      ((x =/= o) &&& (y === o)) &&& ((!! false) === is);
      Fresh.two (fun x' -> fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y' is))]
  let rec le_false x y = conde [(x =/= o) &&& (y === o); Fresh.two (fun x' -> fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le_false x' y'))]
  let rec le_true x y = conde [x === o; Fresh.two (fun x' -> fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le_true x' y'))]
