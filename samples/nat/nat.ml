module L = List
         
open GT
open Printf
open OCanren
open OCanren.Std
open Nat

let rec le x y is = conde [ 
  (x === o) &&& (Bool.truo === is);
  (x =/= o) &&& (y === o) &&& (Bool.falso === is);
  Fresh.two (fun x' y' -> 
    (x === succ x') &&&
    (y === succ y') &&& 
    (le x' y' is));
]

let _ =
  L.iter (fun (x, y) -> printf "x = %s y = %s\n" x y) 
  @@ Stream.take ~n:(10) 
  @@ run qr 
    (fun x y -> le x y Bool.falso)
    (fun x y -> (show Nat.logic (x#reify Nat.reify), show Nat.logic (y#reify Nat.reify)));