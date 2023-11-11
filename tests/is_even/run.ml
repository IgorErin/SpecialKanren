module L = List
open Target
open GT
open OCanren
open OCanren.Std

let _ =
  let test f =
    L.iter (fun x -> Printf.printf "x = %s\n%!" x)
    @@ Stream.take ~n:10
    @@ run q (fun x -> f x) (fun x -> show Nat.logic (x#reify Nat.reify))
  in
  test is_even_1false;
  test is_even_1true
;;
