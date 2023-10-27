  $ cat > a.ml <<-EOF 
  > open OCanren
  > open OCanren.Std
  > open Nat
  > 
  > let rec le x y is =
  >  conde
  >     [ x === o &&& (is === !!true)
  >     ; x =/= o &&& (y === o) &&& (!!false === is)
  >     ; Fresh.two (fun x_ y_ -> x === succ x_ &&& (y === succ y_) &&& le x_ y_ is)
  >     ]
  > ;;
  $ cat a.ml
  open OCanren
  open OCanren.Std
  open Nat
  
  let rec le x y is =
   conde
      [ x === o &&& (is === !!true)
      ; x =/= o &&& (y === o) &&& (!!false === is)
      ; Fresh.two (fun x_ y_ -> x === succ x_ &&& (y === succ y_) &&& le x_ y_ is)
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par is -fname le a.ml
  $ cat a.out  
  open OCanren
  open OCanren.Std
  open Nat
  let rec le x y is =
    conde
      [(x === o) &&& (is === (!! true));
      ((x =/= o) &&& (y === o)) &&& ((!! false) === is);
      Fresh.two (fun x_ -> fun y_ -> ((x === (succ x_)) &&& (y === (succ y_))) &&& (le x_ y_ is))]
  let rec le_false x y = conde [(x =/= o) &&& (y === o); Fresh.two (fun x_ -> fun y_ -> ((x === (succ x_)) &&& (y === (succ y_))) &&& (le_false x_ y_))]
  let rec le_true x y = conde [x === o; Fresh.two (fun x_ -> fun y_ -> ((x === (succ x_)) &&& (y === (succ y_))) &&& (le_true x_ y_))]
