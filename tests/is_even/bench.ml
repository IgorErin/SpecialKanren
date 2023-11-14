open Target_bench
open Benchmark
open OCanren.Std.Nat

let run_gen rel n =
  let open OCanren in
  let _ = Stream.take ~n @@ run q (fun x -> rel x) (fun _ -> ()) in
  ()
;;

let run_false_spec = run_gen Target_bench.is_even_false
let run_true_spec = run_gen Target_bench.is_even_false

let run_false =
  let open OCanren in
  run_gen (fun x -> Target_bench.is_even x !!false)
;;

let run_true =
  let open OCanren in
  run_gen (fun x -> Target_bench.is_even x !!true)
;;

let numbers = Base.List.init 3 ~f:(fun index -> Core.Int.pow 10 (Int.add index 1))
let start list = latencyN ~repeat:10 ~style:Nil 30L list

let () =
  let open OCanren in
  numbers
  |> List.iter (fun number ->
    Printf.printf "number = %d \n" number;
    [ "spec_false", (fun () -> run_false_spec number), ()
    ; "false", (fun () -> run_false number), ()
    ]
    |> start
    |> tabulate)
;;

let () =
  let open OCanren in
  numbers
  |> List.iter (fun number ->
    Printf.printf "number = %d \n" number;
    [ "spec_true", (fun () -> run_true_spec number), ()
    ; "true", (fun () -> run_true number), ()
    ]
    |> start
    |> tabulate)
;;
