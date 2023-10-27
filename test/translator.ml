(* open SpecialKanren *)
(*
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
  let items = SpecialKanren.Translator.translate spec_fun spec_par cmt in 
  Pprintast.structure Format.std_formatter items
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
      let false_and_true x y =
        conde [(x =/= (!! true)) &&& (y === (!! false)); y === (!! true)]
      let false_and_true_false y = conde [y === (!! false); y === (!! true)]
      let false_and_true_true y = conde [y === (!! true)] |}]
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
      let spec_fun x y =
        conde
          [(x === (!! true)) &&& (y === (!! true));
          (x === (!! false)) &&& (y === (!! false))]
      let spec_fun_false y = conde [y === (!! false)]
      let spec_fun_true y = conde [y === (!! true)] |}]
;;

let%expect_test _ =
  createt_test "../../../../default/samples/.le.eobjs/native/dune__exe__Le.cmt" "is" "le";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      open Nat
      let rec le x y is =
        conde
          [(x === o) &&& (is === (!! true));
          ((x =/= o) &&& (y === o)) &&& ((!! false) === is);
          Fresh.two
            (fun x' ->
               fun y' ->
                 ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y' is))]
      let rec le_false x y =
        conde
          [(x =/= o) &&& (y === o);
          Fresh.two
            (fun x' ->
               fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y'))]
      let rec le_true x y =
        conde
          [x === o;
          Fresh.two
            (fun x' ->
               fun y' -> ((x === (succ x')) &&& (y === (succ y'))) &&& (le x' y'))] |}]
;;

let%expect_test _ =
  createt_test
    "../../../../default/samples/.rec_call.eobjs/native/dune__exe__Rec_call.cmt"
    "is"
    "first";
  [%expect
    {|
      open OCanren
      open OCanren.Std
      let rec first x is =
        conde
          [(x === Nat.o) &&& (is === (!! false));
          Fresh.one (fun x -> first x (!! true))]
      let rec first_false x = conde [x === Nat.o]
      let rec first_true x = conde [Fresh.one (fun x -> first x)] |}]
;;

let%expect_test _ =
  createt_test
    "../../../../default/samples/.subs.eobjs/native/dune__exe__Subs.cmt"
    "is"
    "myfun";
  [%expect
    {|
      open OCanren
      open OCanren.Std.Nat
      let rel x y is = conde [(x === o) &&& (is === (!! false)); y === o]
      let myfun x y is =
        conde
          [(x === o) &&& (is === (!! true));
          (y === (succ o)) &&& (is === (!! false));
          rel x y is]
      let myfun_false x y = conde [y === (succ o); rel x y (!! false)]
      let myfun_true x y = conde [x === o; rel x y (!! true)] |}]
;;

let%expect_test _ =
  createt_test
    "../../../../default/samples/.opt.eobjs/native/dune__exe__Opt.cmt"
    "opt"
    "is_none";
  [%expect
    {|
      open OCanren
      let is_none x opt =
        conde
          [(x === (!! true)) &&& (opt === (!! None));
          (x === (!! false)) &&& (opt === (!! (Some 0)))]
      let is_none_None x = conde [x === (!! true)]
      let is_none_Some x = conde [x === (!! false)]|}]
;; *)
