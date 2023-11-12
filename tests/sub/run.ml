module L = List
open Target
open GT
open OCanren
open OCanren.Std

let show_nat x = show Nat.logic (x#reify Nat.reify)

let _ =
  let test f =
    L.iter (fun (x, y) -> Printf.printf "x = %s, y = %s\n%!" x y)
    @@ Stream.take ~n:10
    @@ run
         qr
         (fun x y -> f x y)
         (fun x y ->
           show Nat.logic (x#reify Nat.reify), show Nat.logic (y#reify Nat.reify))
  in
  test sub_None
;;

let _ =
  let test f =
    L.iter (fun (x, y, z) -> Printf.printf "%s - %s = %s\n%!" x y z)
    @@ Stream.take ~n:10
    @@ run
         qrs
         (fun x y z -> f x y z)
         (fun x y z ->
           ( show Nat.logic (x#reify Nat.reify)
           , show Nat.logic (y#reify Nat.reify)
           , show Nat.logic (z#reify Nat.reify) ))
  in
  test sub_Some
;;
