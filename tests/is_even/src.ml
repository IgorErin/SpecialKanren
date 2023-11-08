open OCanren
open OCanren.Std
open OCanren.Std.Nat 

let rec is_even n res =
  conde
    [ n === !!O &&& (res === !!true)
    ; Fresh.two (fun pred_n not_res ->
        (n === !! (S pred_n)) &&&
          (conde
             [ res === !!true &&& (not_res === !!false)
             ; res === !!false &&& (not_res === !!true)
             ]) &&&
          (is_even pred_n not_res))
    ]
;;
