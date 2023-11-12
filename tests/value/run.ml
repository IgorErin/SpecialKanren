module L = List
open Value
open GT
open OCanren
open OCanren.Std

let _ =
  let test name f =
    Printf.printf "%s \n" name;
    L.iter (fun (x, y) -> Printf.printf "default = %s result = %s\n" x y)
    @@ Stream.take ~n:10
    @@ run
         qr
         (fun x y -> f x y)
         (fun x y ->
           show Nat.logic (x#reify Nat.reify), show Nat.logic (y#reify Nat.reify))
  in
  test "second test" value_None;
  let test name f =
    Printf.printf "%s \n" name;
    L.iter (fun (x, y, z) -> Printf.printf "opt = %s default = %s result = %s\n" x y z)
    @@ Stream.take ~n:10
    @@ run
         qrs
         (fun x y z -> f x y z)
         (fun x y z ->
           ( show Nat.logic (x#reify Nat.reify)
           , show Nat.logic (y#reify Nat.reify)
           , show Nat.logic (z#reify Nat.reify) ))
  in
  test "first test" value_Some
;;
