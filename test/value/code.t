  $ cat src.ml
  open OCanren
  
  let value opt default result =
    conde [ opt === !!None &&& (result === default); opt === !!(Some result) ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname value src.ml
  $ ocamlformat --enable-outside-detected-project a.out 
  open OCanren
  
  let value opt default result =
    conde [ opt === !!None &&& (result === default); opt === !!(Some result) ]
  
  let rec value_None default result = result === default
  and value_Some constarg0 default result = constarg0 === result
