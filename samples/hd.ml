open OCanren
open OCanren.Std.List

let rec hd_opt l res =
  conde
    [ l === !!Nil &&& (res === !!None)
    ; Fresh.two (fun hd tl -> l === !!(Cons (hd, tl)) &&& (res === !!(Some hd)))
    ]
;;
