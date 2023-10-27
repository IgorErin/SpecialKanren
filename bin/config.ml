let tool_name = "SpecialCanren"

type ff =
  | Cmt
  | Ml

type t =
  { mutable verbose : bool
  ; mutable path : string option
  ; mutable param : string option
  ; mutable fname : string option
  ; mutable include_dirs : string list
  ; mutable opens : string list
  ; mutable ff : ff option
  ; mutable out : string option
  }

let default =
  { verbose = false
  ; path = None
  ; param = None
  ; fname = None
  ; include_dirs = []
  ; opens = []
  ; ff = None
  ; out = None
  }
;;

let verbose () = default.verbose
let verbose_on () = default.verbose <- true
let set_path p = default.path <- Some p
let set_param p = default.param <- Some p
let set_fname n = default.fname <- Some n
let add_dir d = default.include_dirs <- d :: default.include_dirs
let add_open o = default.opens <- o :: default.opens
let cmt () = default.ff <- Some Cmt
let ml () = default.ff <- Some Ml
let set_out name = default.out <- Some name
let get_out () = default.out

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

let ff () =
  match default.ff with
  | Some x -> x
  | None -> failwith "No file format specified"
;;
