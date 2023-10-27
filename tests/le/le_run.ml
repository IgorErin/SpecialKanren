module L = List
open Le
open GT
open OCanren
open OCanren.Std

let _ =
  let test name f =
    Printf.printf "%s \n" name;
    L.iter (fun (x, y) -> Printf.printf "x = %s y = %s\n" x y)
    @@ Stream.take ~n:10
    @@ run
         qr
         (fun x y -> f x y)
         (fun x y ->
           show Nat.logic (x#reify Nat.reify), show Nat.logic (y#reify Nat.reify))
  in
  test "first test" le_false;
  test "second test" le_true
;;
