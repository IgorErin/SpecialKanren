  $ cat src.ml
  open OCanren
  open OCanren.Std
  
  let rel opt bl =
    conde
      [ Fresh.one (fun x ->
          x
          === opt
          &&& conde
                [ Fresh.two (fun f s ->
                    opt === !!(Some f) &&& (bl === f) &&& (opt === !!(Some s)))
                ; Fresh.two (fun m s ->
                    opt === !!(Some m) &&& (bl === m) &&& (opt === !!(Some s)))
                ; bl === !!false &&& (opt === !!None)
                ])
      ]
  ;;
  $ SpecialKanren -ml -o a.out -par opt -fname rel src.ml
  
  
   new disj 
   && Fresh (x ) && x===None () && bl===false ()
  
   new disj 
   && Fresh (x ) && x===Some (new_var0) && Fresh (s f ) && new_var0===f && bl===f && new_var0===s
   new disj 
   && Fresh (x ) && x===Some (new_var0) && Fresh (s m ) && new_var0===m && bl===m && new_var0===s
  $ cat a.out  
  open OCanren
  open OCanren.Std
  let rel opt bl =
    conde
      [Fresh.one
         (fun x ->
            (x === opt) &&&
              (conde
                 [Fresh.two (fun f -> fun s -> ((opt === (!! (Some f))) &&& (bl === f)) &&& (opt === (!! (Some s))));
                 Fresh.two (fun m -> fun s -> ((opt === (!! (Some m))) &&& (bl === m)) &&& (opt === (!! (Some s))));
                 (bl === (!! false)) &&& (opt === (!! None))]))]
  let rel_None bl = conde [Fresh.one (fun x -> (x === opt) &&& (conde [bl === (!! false)]))]
  let rel_Some bl = conde [Fresh.one (fun x -> (x === opt) &&& (conde [Fresh.two (fun f -> fun s -> bl === f); Fresh.two (fun m -> fun s -> bl === m)]))]
