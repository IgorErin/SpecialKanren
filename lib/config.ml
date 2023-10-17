type t =
  { mutable verbose : bool
  ; mutable path : string option
  ; mutable param : string option
  ; mutable variant : string option
  ; mutable fname : string option 
  }

let default = { verbose = false; path = None; param = None; variant = None; fname = None }
let verbose () = default.verbose
let verbose_on () = default.verbose <- true 
let set_path p = default.path <- Some p
let set_param p = default.param <- Some p
let set_variant v = default.variant <- Some v
let set_fname n = default.fname <- Some n

let path () =
  match default.path with
  | Some x -> x
  | _ -> failwith "No path specified"
;;

let param () =
  match default.param with
  | Some x -> x
  | None -> failwith "No param specified"
;;

let variant () =
  match default.variant with
  | Some x -> x
  | None -> failwith "No variant specified"
;;

let fname () = 
  match default.fname with 
  | Some x -> x 
  | None -> failwith "No fname specified"
