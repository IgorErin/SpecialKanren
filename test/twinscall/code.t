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
  open OCanren.Std
  open OCanren.Std.Nat
  
  let twin fst snd thd x =
    x
    === !!true
    &&& conde
          [ fst === !!false &&& (snd === !!false) &&& (thd === !!false)
          ; fst === !!false &&& (snd === !!false) &&& (thd === !!true)
          ; fst === !!false &&& (snd === !!true) &&& (thd === !!false)
          ; fst === !!false &&& (snd === !!true) &&& (thd === !!true)
          ; fst === !!true &&& (snd === !!false) &&& (thd === !!false)
          ; fst === !!true &&& (snd === !!false) &&& (thd === !!true)
          ; fst === !!true &&& (snd === !!true) &&& (thd === !!false)
          ; fst === !!true &&& (snd === !!true) &&& (thd === !!true)
          ]
  ;;
  
  let twins x is =
    conde
      [ twin !!false !!false is x; twin !!false !!false is x; twin !!false !!false is x ]
  ;;
  
  let rec twin_false_false_true x = x === !!true
  and twin_false_false_false x = x === !!true
  
  and twins_false x =
    conde [ twin_false_false_false x; twin_false_false_false x; twin_false_false_false x ]
  
  and twins_true x =
    conde [ twin_false_false_true x; twin_false_false_true x; twin_false_false_true x ]
  ;;
