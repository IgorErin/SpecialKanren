  $ cat > a.ml <<-EOF 
  > open OCanren
  > 
  > let is_none x opt =
  >  conde [ x === !!true &&& (opt === !!None); x === !!false &&& (opt === !!(Some 0)) ]
  > ;;
  $ cat a.ml
  open OCanren
  
  let is_none x opt =
   conde [ x === !!true &&& (opt === !!None); x === !!false &&& (opt === !!(Some 0)) ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname is_none a.ml
  $ cat a.out  
  open OCanren
  let is_none x opt = conde [(x === (!! true)) &&& (opt === (!! None)); (x === (!! false)) &&& (opt === (!! (Some 0)))]
  let is_none_None x = conde [x === (!! true)]
  let is_none_Some x = conde [x === (!! false)]
