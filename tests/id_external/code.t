  $ cat src.ml
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let fve_id fst snd = fst === snd
  let for_id fst snd = fst =/= snd
  
  let snd_id flag =
    conde
      [ flag === !!true &&& for_id flag !!false; flag === !!false &&& for_id flag !!true ]
  ;;
  
  let thd_id flag =
    conde
      [ flag === !!true &&& for_id flag !!true; flag === !!false &&& fve_id flag !!false ]
  ;;
  
  let id flag = conde [ flag === !!true &&& snd_id flag; flag === !!false &&& thd_id flag ]
  $ SpecialKanren -ml -o a.out -par flag -fname id src.ml
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  open OCanren.Std
  open OCanren.Std.Nat
  
  let fve_id fst snd = fst === snd
  let for_id fst snd = fst =/= snd
  
  let snd_id flag =
    conde
      [
        flag === !!true &&& for_id flag !!false;
        flag === !!false &&& for_id flag !!true;
      ]
  
  let thd_id flag =
    conde
      [
        flag === !!true &&& for_id flag !!true;
        flag === !!false &&& fve_id flag !!false;
      ]
  
  let id flag =
    conde [ flag === !!true &&& snd_id flag; flag === !!false &&& thd_id flag ]
  
  let rec fve_id_false_false = failwith "Reduced"
  and for_id_true_false = failwith "Reduced"
  and thd_id_false = fve_id_false_false
  and snd_id_true = for_id_true_false
  and id_false = thd_id_false
  and id_true = snd_id_true
