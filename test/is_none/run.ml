module L = List
open Target
open GT
open OCanren
open Tester
open OCanren.Std

let show_ans x = [%show: Bool.logic] () x
let reify_ans x = Bool.reify x

let () =
  run_r reify_ans show_ans 10 q qh ("is none none", fun result -> is_none_None result)
;;

let () =
  run_r
    reify_ans
    show_ans
    10
    q
    qh
    ("is none some ", fun is -> Fresh.one (fun x -> is_none_Some is x))
;;
