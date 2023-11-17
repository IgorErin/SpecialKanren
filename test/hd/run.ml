module L = List
open Target
open GT
open OCanren
open OCanren.Std
open Tester

let show_ans x = [%show: Nat.logic List.logic] () x
let reify_ans x = List.reify Nat.reify x
let singleton x = List.(x % List.nil ())
let singletonvalue ls value = ls === singleton value
let () = run_r reify_ans show_ans 100 q qh ("hd of empty list", fun q -> hd_opt_None q)

let () =
  run_r
    reify_ans
    show_ans
    100
    qr
    qrh
    ( "hd and tl"
    , fun h list ->
        Fresh.one (fun value -> singletonvalue h value &&& hd_opt_Some list value) )
;;
