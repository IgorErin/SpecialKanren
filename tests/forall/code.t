  $ cat src.ml
  open OCanren
  open OCanren.Std.List
  
  let rec for_allo pred l res =
    conde
      [ l === !!Nil &&& (res === !!true)
      ; Fresh.three (fun x xs pred_res ->
          l
          === !!(Cons (x, xs))
          &&& pred x pred_res
          &&& conde
                [ pred_res === !!true &&& for_allo pred xs res
                ; pred_res === !!false &&& (res === !!false)
                ])
      ]
  ;;
  $ SpecialKanren -ml -par res -fname for_allo src.ml -o a.out
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std.List
  
  let rec for_allo pred l res =
    conde
      [
        l === !!Nil &&& (res === !!true);
        Fresh.three (fun x xs pred_res ->
            l
            === !!(Cons (x, xs))
            &&& pred x pred_res
            &&& conde
                  [
                    pred_res === !!true &&& for_allo pred xs res;
                    pred_res === !!false &&& (res === !!false);
                  ]);
      ]
  
  let rec for_allo_false pred l =
    Fresh.two (fun x xs ->
        l === !!(Cons (x, xs)) &&& (pred x !!true &&& for_allo_false pred xs))
    ||| Fresh.two (fun x xs -> l === !!(Cons (x, xs)) &&& pred x !!false)
  
  and for_allo_true pred l =
    l === !!Nil
    ||| Fresh.two (fun x xs ->
            l === !!(Cons (x, xs)) &&& (pred x !!true &&& for_allo_true pred xs))
TODO(l)
