open SpecialKanren

let () =
  let () = Clflags.recursive_types := true in
  ()
;;

(* copypast from main. rework *)
let read_cmt path =
  let open Tast_mapper in
  let map_env s =
    let mapper = { default with env = (fun _ env -> Envaux.env_of_only_summary env) } in
    mapper.structure mapper s
  in
  let infos = Cmt_format.read_cmt path in
  infos.Cmt_format.cmt_annots
  |> function
  | Cmt_format.Implementation cmt ->
    Load_path.init infos.cmt_loadpath;
    map_env cmt
  | _ -> failwith "Implementation expected"
;;

let createt_test path par fname =
  let cmt = read_cmt path in
  let spec_par = Predicate.par_of_string par in
  let spec_fun = Predicate.fun_of_string fname in
  SpecialKanren.Translator.translate spec_fun spec_par cmt
;;

(* end of copypast *)

let%expect_test _ =
  createt_test
    "../../../../default/samples/.false_and_true.eobjs/native/dune__exe__False_and_true.cmt"
    "x"
    "false_and_true";
  [%expect
    {|
      open OCanren
      let false_and_true y = conde [y === (!! false); y === (!! true)]
      and false_and_true y = conde [y === (!! true)] |}]
;;

let%expect_test _ =
  createt_test
    "../../../../default/samples/.eq.eobjs/native/dune__exe__Eq.cmt"
    "x"
    "spec_fun";
  [%expect
    {|
      open OCanren
      let some_fun x = x
      let spec_fun y = conde [y === (!! false)]
      and spec_fun y = conde [y === (!! true)] |}]
;;

let%expect_test _ =
  createt_test "../../../../default/samples/.le.eobjs/native/dune__exe__Le.cmt" "is" "le";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      open Nat
      let rec le x y =
        conde
          [(x =/= o) &&& (y === o);
          Fresh.two
            (fun x' ->
               fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y'))]
      and le x y =
        conde
          [x === o;
          Fresh.two
            (fun x' ->
               fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y'))] |}]
;;

(* let%expect_test _ =
   createt_test
   "../../../../default/samples/.rec_call.eobjs/native/dune__exe__Rec_call.cmt"
   "is"
   "first";
   [%expect
    {|
      open OCanren
      open OCanren.Std
      let rec first x = conde [] |}]
   ;; *)
