let un_arg = function
  | [ (_, Some arg) ] -> arg
  | _ -> assert false
;;

let bin args =
  match args with
  | [ fst; snd ] -> fst, snd
  | _ -> assert false
;;

let bin_args args =
  match args with
  | [ (_, Some fexp); (_, Some sexp) ] -> fexp, sexp
  | _ -> assert false
;;

let lb_arg = function
  | [ (lb, Some arg) ] -> lb, arg
  | _ -> assert false
;;
