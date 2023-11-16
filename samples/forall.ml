open OCanren
open OCanren.Std.List

let rec for_allo pred l res =
  conde
    [ l === !!Nil &&& (res === !!true)
    ; Fresh.three (fun x xs pred_res ->
        l
        === !!(Cons (x, xs))
        &&& pred x pred_res
        &&& conde
              [ pred_res === !!true &&& for_allo pred xs res
              ; pred_res === !!false &&& (res === !!false)
              ])
    ]
;;
