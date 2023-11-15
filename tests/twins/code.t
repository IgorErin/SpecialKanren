  $ cat src.ml
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let twin fst snd thd =
    conde
      [ fst === !!false &&& (snd === !!false) &&& (thd === !!false)
      ; fst === !!false &&& (snd === !!false) &&& (thd === !!true)
      ; fst === !!false &&& (snd === !!true) &&& (thd === !!false)
      ; fst === !!false &&& (snd === !!true) &&& (thd === !!true)
      ; fst === !!true &&& (snd === !!false) &&& (thd === !!false)
      ; fst === !!true &&& (snd === !!false) &&& (thd === !!true)
      ; fst === !!true &&& (snd === !!true) &&& (thd === !!false)
      ; fst === !!true &&& (snd === !!true) &&& (thd === !!true)
      ]
  ;;
  
  let twins is =
    conde [ twin !!false !!false is; twin !!false !!false is; twin !!false !!false is ]
  ;;
  $ SpecialKanren -ml -o a.out -par is -fname twins src.ml
  $ ocamlformat --enable-outside-detected-project a.out
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let twin fst snd thd =
    conde
      [
        fst === !!false &&& (snd === !!false) &&& (thd === !!false);
        fst === !!false &&& (snd === !!false) &&& (thd === !!true);
        fst === !!false &&& (snd === !!true) &&& (thd === !!false);
        fst === !!false &&& (snd === !!true) &&& (thd === !!true);
        fst === !!true &&& (snd === !!false) &&& (thd === !!false);
        fst === !!true &&& (snd === !!false) &&& (thd === !!true);
        fst === !!true &&& (snd === !!true) &&& (thd === !!false);
        fst === !!true &&& (snd === !!true) &&& (thd === !!true);
      ]
  
  let twins is =
    conde
      [
        twin !!false !!false is; twin !!false !!false is; twin !!false !!false is;
      ]
  
  let rec twin_false_false_true = failwith "Reduced"
  and twin_false_false_false = failwith "Reduced"
  
  and twins_false =
    conde
      [ twin_false_false_false; twin_false_false_false; twin_false_false_false ]
  
  and twins_true =
    conde [ twin_false_false_true; twin_false_false_true; twin_false_false_true ]
