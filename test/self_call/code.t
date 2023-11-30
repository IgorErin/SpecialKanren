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
  
  let rec self is = conde [ self is; is === !!false ]
  
  let rec self1 is =
    conde [ self1 !!false &&& (is === !!true); self1 !!true &&& (is === !!false) ]
  ;;
  
  let rec self1_0true = self1_0false
  and self1_0false = self1_0true
