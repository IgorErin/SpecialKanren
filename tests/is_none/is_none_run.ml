module L = List
open Is_none
open GT
open OCanren
open OCanren.Std

let _ =
  let test f =
    L.iter (fun x -> Printf.printf "x = %s\n%!" x)
    @@ Stream.take ~n:10
    @@ run q (fun x -> f x) (fun x -> show Bool.logic (x#reify Bool.reify))
  in
  test is_none_None;
  test is_none_Some
;;
