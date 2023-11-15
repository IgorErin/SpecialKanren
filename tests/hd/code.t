  $ cat src.ml
  open OCanren
  open OCanren.Std.List
  
  let rec hd_opt l res =
    conde
      [ l === !!Nil &&& (res === !!None)
      ; Fresh.two (fun hd tl -> l === !!(Cons (hd, tl)) &&& (res === !!(Some hd)))
      ]
  ;;
  $ SpecialKanren -ml -par res -fname hd_opt src.ml -o a.out
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std.List
  
  let rec hd_opt l res =
    conde
      [
        l === !!Nil &&& (res === !!None);
        Fresh.two (fun hd tl -> l === !!(Cons (hd, tl)) &&& (res === !!(Some hd)));
      ]
  
  let rec hd_opt_None l = l === !!Nil
  
  and hd_opt_Some l some_arg_0 =
    Fresh.two (fun hd tl -> l === !!(Cons (hd, tl)) &&& (some_arg_0 === hd))
TODO(list error)
$ SpecialKanren -ml -par l -fname hd_opt src.ml -o a.out
$ ocamlformat --enable-outside-detected-project a.out
