let tool_name = "SpecialCanren"

type t =
  {  mutable verbose : bool
  ; mutable path : string option
  ; mutable param : string option
  ; mutable fname : string option
  ; mutable include_dirs : string list
  ; mutable opens : string list
  }

let default =
  {
     verbose = false
  ; path = None
  ; param = None
  ; fname = None
  ; include_dirs = []
  ; opens = []
  }
;;

let verbose () = default.verbose
let verbose_on () = default.verbose <- true
let set_path p = default.path <- Some p
let set_param p = default.param <- Some p
let set_fname n = default.fname <- Some n
let add_dir d = default.include_dirs <- d :: default.include_dirs
let add_open o = default.opens <- o :: default.opens

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

let fname () =
  match default.fname with
  | Some x -> x
  | None -> failwith "No fname specified"
;;

let include_dirs () = default.include_dirs
let opens () = default.opens
