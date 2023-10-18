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

let create_par_predicate name =
  object
    method exp =
      let open Patterns in
      let open Ocanren_patterns in
      parse_bool Gen.(exp_by_texp_ident [ str name ])

    method ident x = String.equal name @@ Ident.name x
  end
;;

(* full path for now *)
let create_var_predicate path =
  let open Patterns in
  let open Ocanren_patterns in
  let spec_variant =
    path
    |> String.split_on_char '.'
    |> List.map Patterns.Gen.str
    |> fun x -> exp_by_texp_ident x
  in
  object
    method exp = parse_bool spec_variant
  end
;;

let create_fun_predicate name =
  let open Patterns in
  let open Gen in
  object
    method ident x = String.equal name @@ Ident.name x

    method pat =
      let id = Patterns.ident name in
      let pat_desc = Pattern_desc.tpat_var id drop in
      let pat_data = Patterns.pattern_data pat_desc drop drop drop drop drop in
      parse_bool pat_data
  end
;;

let createt_test path par var fname =
  let cmt = read_cmt_impl path in
  let spec_par = create_par_predicate par in
  let spec_var = create_var_predicate var in
  let spec_fun = create_fun_predicate fname in
  SpecialKanren.Translator.translate spec_par spec_var spec_fun cmt
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
               fun y' ->
                 ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y' is))] |}]
;;
