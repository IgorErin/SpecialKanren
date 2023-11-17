module L = List
open Target
open GT
open OCanren
open OCanren.Std
open Tester

let show_ans x = [%show: Nat.logic] () x
let reify_ans x = Nat.reify x
let () = run_r reify_ans show_ans 10 q qh ("is even false", fun q -> is_even_1false q)
let () = run_r reify_ans show_ans 10 q qh ("is even true", fun q -> is_even_1true q)
