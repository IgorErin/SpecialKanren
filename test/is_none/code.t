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
  
  let is_none x opt =
    conde
      [ x === !!true &&& (opt === !!None)
      ; x === !!false &&& Fresh.one (fun x' -> opt === !!(Some x'))
      ]
  ;;
  
  let rec is_none_1Some x constarg0 = x === !!false
  and is_none_1None x = x === !!true
