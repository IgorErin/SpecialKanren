module L = List
open Target
open GT
open OCanren
open OCanren.Std
open Tester

let show_ans x = [%show: Nat.logic] () x
let reify_ans x = Nat.reify x
let () = run_r reify_ans show_ans 10 qr qrh ("le false", fun fst snd -> le_false fst snd)
let () = run_r reify_ans show_ans 10 qr qrh ("le true", fun fst snd -> le_true fst snd)
