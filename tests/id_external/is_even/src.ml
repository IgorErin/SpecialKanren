open OCanren
open OCanren.Std
open OCanren.Std.Nat

let fve_id fst snd = fst === snd
let for_id fst snd = fst =/= snd

let snd_id flag =
  conde
    [ flag === !!true &&& for_id flag !!false; flag === !!false &&& for_id flag !!true ]
;;

let thd_id flag =
  conde
    [ flag === !!true &&& for_id flag !!true; flag === !!false &&& fve_id flag !!false ]
;;

let id flag = conde [ flag === !!true &&& snd_id flag; flag === !!false &&& thd_id flag ]
