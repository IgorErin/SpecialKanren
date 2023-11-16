open Target_bench
open Benchmark
open OCanren.Std.Nat

let run_Some n =
  let open OCanren in
  let _ =
    Stream.take ~n
    @@ run
         qrs
         (fun fst snd thd -> Target_bench.sub fst snd !!(Some thd))
         (fun _ _ _ -> ())
  in
  ()
;;

let none = OCanren.Std.Option.none ()

let run_None n =
  let open OCanren in
  let _ =
    Stream.take ~n
    @@ run qrs (fun fst snd thd -> Target_bench.sub fst snd none) (fun _ _ _ -> ())
  in
  ()
;;

let run_Some_spec n =
  let open OCanren in
  let _ =
    Stream.take ~n
    @@ run qrs (fun fst snd thd -> Target_bench.sub_Some fst snd thd) (fun _ _ _ -> ())
  in
  ()
;;

let run_None_spec n =
  let open OCanren in
  let _ =
    Stream.take ~n
    @@ run qr (fun fst snd -> Target_bench.sub_None fst snd) (fun _ _ _ -> ())
  in
  ()
;;

let numbers = Base.List.init 3 ~f:(fun index -> Core.Int.pow 5 (Int.add index 1))
let start = latencyN ~repeat:2 ~style:Nil 30L

let () =
  numbers
  |> List.iter (fun count ->
    Printf.printf "count: %d\n" count;
    [ "spec_some", (fun () -> run_Some_spec count), ()
    ; "some", (fun () -> run_Some count), ()
    ]
    |> start
    |> tabulate)
;;

let () =
  numbers
  |> List.iter (fun count ->
    Printf.printf "count: %d\n" count;
    [ "spec_none", (fun () -> run_None_spec count), ()
    ; "none", (fun () -> run_None count), ()
    ]
    |> start
    |> tabulate)
;;
