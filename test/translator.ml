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

let createt_test path par var =
  let cmt = read_cmt_impl path in
  let spec_var =
    object
      method exp =
        let open Patterns in
        let open Ocanren_patterns in
        parse_bool Gen.(exp_by_texp_ident [ str par ])

      method ident x = String.equal par @@ Ident.name x
    end
  in
  let spec_variant =
    let open Patterns in
    let open Ocanren_patterns in
    let spec_variant =
      var
      |> String.split_on_char '.'
      |> List.map Patterns.Gen.str
      |> fun x -> exp_by_texp_ident x
    in
    object
      method exp = parse_bool spec_variant
    end
  in
  SpecialKanren.Translator.translate spec_var spec_variant cmt
;;

(* end of copypast *)

let%expect_test _ =
  createt_test
    "../../../../default/samples/.false_and_true.eobjs/native/dune__exe__False_and_true.cmt"
    "x"
    "OCanren.Std.Bool.truo";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      let false_and_true y = conde [y === Bool.truo] |}]
;;
