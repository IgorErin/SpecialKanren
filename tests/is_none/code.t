  $ cat > a.ml <<-EOF 
  > open OCanren
  > 
  > let is_none x opt =
  >  conde [ x === !!true &&& (opt === !!None); x === !!false &&& (Fresh.one (fun x_ -> opt === !!(Some x_))) ]
  > ;;
  $ cat a.ml
  open OCanren
  
  let is_none x opt =
   conde [ x === !!true &&& (opt === !!None); x === !!false &&& (Fresh.one (fun x_ -> opt === !!(Some x_))) ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname is_none a.ml
  $ cat a.out  
  open OCanren
  let is_none x opt = conde [(x === (!! true)) &&& (opt === (!! None)); (x === (!! false)) &&& (Fresh.one (fun x_ -> opt === (!! (Some x_))))]
  let is_none_None x = conde [x === (!! true)]
  let is_none_Some x = conde [x === (!! false)]
