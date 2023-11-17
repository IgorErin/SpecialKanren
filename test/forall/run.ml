module L = List
open Target
open GT
open OCanren
open OCanren.Std

let cond x is =
  conde [ is === !!true &&& (x === Nat.one); is === !!false &&& (x === Nat.zero) ]
;;

let show_ans x = [%show: Nat.logic List.logic] () x
let reify = Std.List.reify Nat.reify
let to_string r = show_ans @@ r#reify reify

let _ =
  let () =
    L.iter (fun x -> Printf.printf "%s\n" x)
    @@ Stream.take ~n:10
    @@ run q (fun q -> for_allo_2false cond q) (fun r -> show_ans @@ r#reify reify)
  in
  let () =
    L.iter (fun x -> Printf.printf "%s\n" x)
    @@ Stream.take ~n:10
    @@ run q (fun q -> for_allo_2true cond q) (fun r -> show_ans @@ r#reify reify)
  in
  ()
;;
