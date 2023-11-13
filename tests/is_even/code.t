  $ cat src.ml
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let rec is_even n res =
    conde
      [ n === !!O &&& (res === !!true)
      ; Fresh.two (fun pred_n not_res ->
          n
          === !!(S pred_n)
          &&& conde
                [ res === !!true &&& (not_res === !!false)
                ; res === !!false &&& (not_res === !!true)
                ]
          &&& is_even pred_n not_res)
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par res -fname is_even src.ml
  $ cat a.out  
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  let rec is_even n res =
    conde
      [(n === (!! O)) &&& (res === (!! true));
      Fresh.two
        (fun pred_n ->
           fun not_res ->
             ((n === (!! (S pred_n))) &&& (conde [(res === (!! true)) &&& (not_res === (!! false)); (res === (!! false)) &&& (not_res === (!! true))])) &&& (is_even pred_n not_res))]
  let rec is_even_false n = Fresh.two (fun pred_n -> fun not_res -> (n === (!! (S (pred_n)))) &&& ((not_res === (!! true)) &&& (is_even_true pred_n)))
  and is_even_true n = (n === (!! O)) ||| (Fresh.two (fun pred_n -> fun not_res -> (n === (!! (S (pred_n)))) &&& ((not_res === (!! false)) &&& (is_even_false pred_n))))
