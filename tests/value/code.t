  $ cat > a.ml <<-EOF 
  > open OCanren
  > 
  > let value opt default result = 
  > conde [ 
  >   (opt === !! None) &&& (result === default);
  >   Fresh.one (fun x -> (opt === !! (Some x)&&& (result === x)) )
  >  ]
  > 
  $ cat a.ml
  open OCanren
  
  let value opt default result = 
  conde [ 
    (opt === !! None) &&& (result === default);
    Fresh.one (fun x -> (opt === !! (Some x)&&& (result === x)) )
   ]
  
  $ SpecialKanren -ml -o a.out -par opt -fname value a.ml
  $ cat a.out  
  open OCanren
  let value opt default result = conde [(opt === (!! None)) &&& (result === default); Fresh.one (fun x -> (opt === (!! (Some x))) &&& (result === x))]
  let value_None default result = conde [result === default]
  let value_Some default result = conde [Fresh.one (result === x)]
