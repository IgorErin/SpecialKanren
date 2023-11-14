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

let () =
  Base.List.init 4 ~f:(fun index -> Core.Int.pow 5 (Int.add index 1))
  |> List.iter (fun count ->
    Printf.printf "count: %d\n" count;
    let res =
      latencyN
        ~repeat:1
        ~style:Nil
        10L
        [ "spec_some", (fun () -> run_Some_spec count), ()
        ; "just_some", (fun () -> run_Some count), ()
        ; "spec_none", (fun () -> run_None_spec count), ()
        ; "just_none", (fun () -> run_None count), ()
        ]
    in
    tabulate res)
;;
