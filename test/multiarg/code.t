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
  
  let spec fst snd opt =
    conde
      [ fst === !!true &&& (opt === !!None) &&& (snd === !!true)
      ; fst === !!false &&& (snd === !!false) &&& Fresh.one (fun x -> opt === !!(Some x))
      ]
  ;;
  
  let rel is =
    conde
      [ is === !!false &&& spec !!true !!true !!None
      ; Fresh.one (fun x -> spec !!false !!false !!(Some x)) &&& (is === !!false)
      ]
  ;;
  
  let rec spec_false_false_Some constarg0 = failwith "Reduced"
  and spec_true_true_None = failwith "Reduced"
  and rel_false = spec_true_true_None ||| Fresh.one (fun x -> spec_false_false_Some x)
  and rel_true = failwith "Reduced"
