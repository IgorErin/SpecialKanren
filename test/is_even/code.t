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
  
  let rec is_even n res =
    conde
      [ n === !!O &&& (res === !!true)
      ; Fresh.two (fun pred_n not_res ->
          n
          === !!(S pred_n)
          &&& conde
                [ res === !!true &&& (not_res === !!false)
                ; res === !!false &&& (not_res === !!true)
                ]
          &&& is_even pred_n not_res)
      ]
  ;;
  
  let rec is_even_1false n =
    Fresh.one (fun pred_n -> n === !!(S pred_n) &&& is_even_1true pred_n)
  
  and is_even_1true n =
    n === !!O ||| Fresh.one (fun pred_n -> n === !!(S pred_n) &&& is_even_1false pred_n)
  ;;
