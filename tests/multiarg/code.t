  $ cat src.ml
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let spec fst snd opt =
    conde
      [ fst === !!true &&& (opt === !!None) &&& (snd === !!true)
      ; fst === !!false &&& (snd === !!false) &&& Fresh.one (fun x -> opt === !!(Some x))
      ]
  ;;
  
  let rel is =
    conde
      [ is === !!false &&& spec !!true !!true !!None
      ; Fresh.one (fun x -> spec !!false !!false !!(Some x)) &&& (is === !!false)
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par is -fname rel src.ml
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let spec fst snd opt =
    conde
      [
        fst === !!true &&& (opt === !!None) &&& (snd === !!true);
        fst === !!false &&& (snd === !!false)
        &&& Fresh.one (fun x -> opt === !!(Some x));
      ]
  
  let rel is =
    conde
      [
        is === !!false &&& spec !!true !!true !!None;
        Fresh.one (fun x -> spec !!false !!false !!(Some x)) &&& (is === !!false);
      ]
  
  let rec spec_true_true_None = failwith "Reduced"
  and spec_false_false_Some new_var0 = Fresh.one (fun x -> new_var0 === x)
  
  and rel_false =
    spec_true_true_None ||| Fresh.one (fun x -> spec_false_false_Some x)
  
  and rel_true = failwith "Reduced"
