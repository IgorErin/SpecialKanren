module L = List
open Target
open GT
open OCanren
open OCanren.Std
open Tester

let show_ans x = [%show: person_logic List.logic] () x
let reify_ans x = List.reify person_reify x
let () = run_r reify_ans show_ans 100 q qh ("true", fun q -> checkAnswer q !!true)
let () = run_r reify_ans show_ans 100 q qh ("spec true", fun q -> checkAnswer_1true q)
let () = run_r reify_ans show_ans 100 q qh ("false", fun q -> checkAnswer q !!false)
let () = run_r reify_ans show_ans 100 q qh ("spec false", fun q -> checkAnswer_1false q)
