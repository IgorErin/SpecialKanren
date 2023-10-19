open SpecialKanren

(* copypast from main. rework *)

let read_cmt_impl path =
  if Stdlib.Sys.file_exists path
  then (
    let infos = Cmt_format.read_cmt path in
    infos.Cmt_format.cmt_annots
    |> function
    | Cmt_format.Implementation cmt -> cmt
    | _ -> failwith "Implementation expected")
  else failwith "File does not exist"
;;

let createt_test path par var fname =
  let cmt = read_cmt_impl path in
  let spec_par = Predicate.par_of_string par in
  let spec_var = Predicate.var_of_string var in
  let spec_fun = Predicate.fun_of_string fname in
  SpecialKanren.Translator.translate spec_fun spec_par spec_var cmt
;;

(* end of copypast *)

let%expect_test _ =
  createt_test
    "../../../../default/samples/.false_and_true.eobjs/native/dune__exe__False_and_true.cmt"
    "x"
    "OCanren.Std.Bool.truo"
    "false_and_true";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      let false_and_true y = conde [y === Bool.truo] |}]
;;

let%expect_test _ =
  createt_test
    "../../../../default/samples/.eq.eobjs/native/dune__exe__Eq.cmt"
    "x"
    "OCanren.Std.Bool.truo"
    "spec_fun";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      let some_fun x = x
      let spec_fun y = conde [y === Bool.truo] |}]
;;

let%expect_test _ =
  createt_test
    "../../../../default/samples/.le.eobjs/native/dune__exe__Le.cmt"
    "is"
    "OCanren.Std.Bool.truo"
    "le";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      open Nat
      let rec le x y =
        conde
          [x === o;
          Fresh.two
            (fun x' ->
               fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y'))] |}]
;;
