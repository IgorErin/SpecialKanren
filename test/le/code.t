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
  open Nat
  
  let rec le x y is =
    conde
      [ x === !!O &&& (is === !!true)
      ; x =/= !!O &&& (y === !!O) &&& (!!false === is)
      ; Fresh.two (fun x' y' -> x === !!(S x') &&& (y === !!(S y')) &&& le x' y' is)
      ]
  ;;
  
  let rec le_2true x y =
    x
    === !!O
    ||| Fresh.two (fun x' y' -> x === !!(S x') &&& (y === !!(S y') &&& le_2true x' y'))
  
  and le_2false x y =
    x
    =/= !!O
    &&& (y === !!O)
    ||| Fresh.two (fun x' y' -> x === !!(S x') &&& (y === !!(S y') &&& le_2false x' y'))
  ;;
