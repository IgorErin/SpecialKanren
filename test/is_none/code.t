  $ cat is_none.src.ml
  open OCanren
  
  let is_none x opt =
    conde
      [ x === !!true &&& (opt === !!None)
      ; x === !!false &&& Fresh.one (fun x' -> opt === !!(Some x'))
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname is_none is_none.src.ml
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  
  let is_none x opt =
    conde
      [
        x === !!true &&& (opt === !!None);
        x === !!false &&& Fresh.one (fun x' -> opt === !!(Some x'));
      ]
  
  let rec is_none_None x = x === !!true
  and is_none_Some x constarg0 = x === !!false
