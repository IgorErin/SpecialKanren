  $ cat src.ml
  open OCanren
  
  let value opt default result =
    conde [ opt === !!None &&& (result === default); opt === !!(Some result) ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname value src.ml
  $ cat a.out  
  open OCanren
  let value opt default result = conde [(opt === (!! None)) &&& (result === default); opt === (!! (Some result))]
  let rec value_0None default result = result === default
  and value_0Some new_var0 default result = new_var0 === result
