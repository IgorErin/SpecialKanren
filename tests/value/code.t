  $ cat src.ml
  open OCanren
  
  let value opt default result =
    conde [ opt === !!None &&& (result === default); opt === !!(Some result) ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname value src.ml
  $ cat a.out  
  open OCanren
  let value opt default result = conde [(opt === (!! None)) &&& (result === default); opt === (!! (Some result))]
  let value_None result default = result === default
  let value_Some result new_var0 = new_var0 === result
