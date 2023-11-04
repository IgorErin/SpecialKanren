  $ cat value_src.ml
  open OCanren
  
  let value opt default result =
    conde [ opt === !!None &&& (result === default); opt === !!(Some result) ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname value value_src.ml
  
  
   new disj 
   && result===default
  
   new disj 
   && new_var0===result
  $ cat a.out  
  open OCanren
  let value opt default result = conde [(opt === (!! None)) &&& (result === default); opt === (!! (Some result))]
  let value_None default result = conde [result === default]
  let value_Some default result = conde []
