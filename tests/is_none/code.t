  $ cat is_none.src.ml
  open OCanren
  
  let is_none x opt =
    conde
      [ x === !!true &&& (opt === !!None)
      ; x === !!false &&& Fresh.one (fun x' -> opt === !!(Some x'))
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname is_none is_none.src.ml
  
  
   new disj 
   && x===true ()
  
   new disj 
   && x===false () && new_var0===x'
  $ cat a.out  
  open OCanren
  let is_none x opt = conde [(x === (!! true)) &&& (opt === (!! None)); (x === (!! false)) &&& (Fresh.one (fun x' -> opt === (!! (Some x'))))]
  let is_none_None x = conde [x === (!! true)]
  let is_none_Some x = conde [x === (!! false)]
