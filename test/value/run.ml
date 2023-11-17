module L = List
open Target
open GT
open OCanren
open OCanren.Std
open Tester

let show_ans x = [%show: Nat.logic] () x
let reify_ans x = Nat.reify x

let () =
  run_r
    reify_ans
    show_ans
    10
    qrs
    qrsh
    ("value some", fun fst snd result -> value_0Some fst snd result)
;;

let () =
  run_r reify_ans show_ans 10 qr qrh ("value none", fun fst snd -> value_0None fst snd)
;;
