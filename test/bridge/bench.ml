open Target_bench
open Benchmark

let none_gen rel n =
  let open OCanren in
  let _ = Stream.take ~n @@ run q (fun x -> rel x) (fun _ -> ()) in
  ()
;;

let some_gen rel n =
  let open OCanren in
  let _ = Stream.take ~n @@ run qr (fun x y -> rel x y) (fun _ _ -> ()) in
  ()
;;

let run_some =
  let open OCanren in
  some_gen (fun x y -> getAnswer x !!(Some y))
;;

let run_spec_some = some_gen (fun x y -> getAnswer_1Some x y)

let run_none =
  let open OCanren in
  none_gen (fun x -> getAnswer x !!None)
;;

let run_spec_none = none_gen (fun x -> getAnswer_1None x)
let numbers = Base.List.init 3 ~f:(fun index -> Core.Int.pow 10 (Int.add index 1))
let start list = latencyN ~repeat:3 ~style:Nil 10L list

let () =
  let open OCanren in
  numbers
  |> List.iter (fun number ->
    Printf.printf "number = %d\n" number;
    [ "spec_none", (fun () -> run_spec_none number), ()
    ; "none", (fun () -> run_none number), ()
    ]
    |> start
    |> tabulate)
;;

let numbers = Base.List.init 1 ~f:(fun index -> Core.Int.pow 1 (Int.add index 1))
let start list = latencyN ~repeat:3 ~style:Nil 10L list

let () =
  let open OCanren in
  numbers
  |> List.iter (fun number ->
    Printf.printf "number = %d\n" number;
    [ "spec_some", (fun () -> run_spec_some number), ()
    ; "some", (fun () -> run_some number), ()
    ]
    |> start
    |> tabulate)
;;
