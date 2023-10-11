type t =
  { mutable verbose : bool
  ; mutable path : string option
  }

let default = { verbose = false; path = None }
let verbose () = default.verbose
let set_path p = default.path <- Some p
let path () = default.path
