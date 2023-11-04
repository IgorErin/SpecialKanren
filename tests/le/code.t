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
  
  
   new disj 
   && (x=/=O () && y===O ()
   new disj 
   && x===S (x') && y===S (y') && le (x'y'is)
  
   new disj 
   && x===O ()
   new disj 
   && x===S (x') && y===S (y') && le (x'y'is)
  $ cat a.out  
  open OCanren
  open OCanren.Std
  open Nat
  let rec le x y is =
    conde
      [(x === (!! O)) &&& (is === (!! true));
      ((x =/= (!! O)) &&& (y === (!! O))) &&& ((!! false) === is);
      Fresh.two (fun x' -> fun y' -> ((x === (!! (S x'))) &&& (y === (!! (S y')))) &&& (le x' y' is))]
  let rec le_false x y = conde [(x =/= (!! O)) &&& (y === (!! O)); Fresh.two (fun x' -> fun y' -> ((x === (!! (S x'))) &&& (y === (!! (S y')))) &&& (le_false x' y'))]
  let rec le_true x y = conde [x === (!! O); Fresh.two (fun x' -> fun y' -> ((x === (!! (S x'))) &&& (y === (!! (S y')))) &&& (le_true x' y'))]
