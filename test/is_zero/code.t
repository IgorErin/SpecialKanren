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
  open OCanren.Std.Nat
  
  let is_zero num is =
    conde [ num === !!O &&& (is === !!true); num =/= !!O &&& (is === !!false) ]
  ;;
  
  let rec is_zero_1true num = num === !!O
  and is_zero_1false num = num =/= !!O
