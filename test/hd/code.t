  $ cat target.ml
  [@@@ocaml.ppx.context
    { tool_name = "ppx_driver"
    ; include_dirs = []
    ; load_path = []
    ; open_modules = []
    ; for_package = None
    ; debug = false
    ; use_threads = false
    ; use_vmthreads = false
    ; recursive_types = false
    ; principal = false
    ; transparent_modules = false
    ; unboxed_types = false
    ; unsafe_string = false
    ; cookies = [ "library-name", "Samples" ]
    }]
  
  open OCanren
  open OCanren.Std.List
  
  let rec hd_opt l res =
    conde
      [ l === !!Nil &&& (res === !!None)
      ; Fresh.two (fun hd tl -> l === !!(Cons (hd, tl)) &&& (res === !!(Some hd)))
      ]
  ;;
  
  let rec hd_opt_None l = l === !!Nil
  and hd_opt_Some l constarg0 = Fresh.one (fun tl -> l === !!(Cons (constarg0, tl)))
