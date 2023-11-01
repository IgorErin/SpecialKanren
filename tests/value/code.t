  $ cat value_src.ml
  open OCanren
  
  let value opt default result =
    conde
      [ opt === !!None &&& (result === default)
      ; Fresh.one (fun x -> opt === !!(Some x) &&& (result === x))
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname value value_src.ml
  $ cat a.out  
  open OCanren
  let value opt default result = conde [(opt === (!! None)) &&& (result === default); Fresh.one (fun x -> (opt === (!! (Some x))) &&& (result === x))]
  let value_None default result = conde [result === default]
  let value_Some default result = conde [Fresh.one (result === x)]
