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
  
  let rec loe x y res =
    conde
      [ x === !!O &&& (res === !!true)
      ; Fresh.one (fun pred_x ->
          x
          === !!(S pred_x)
          &&& conde
                [ y === !!O &&& (res === !!false)
                ; Fresh.one (fun pred_y -> y === !!(S pred_y) &&& loe pred_x pred_y res)
                ])
      ]
  ;;
  
  let rec add x y z =
    conde
      [ x === !!O &&& (y === z)
      ; Fresh.two (fun pred_x pred_z ->
          x === !!(S pred_x) &&& (z === !!(S pred_z)) &&& add pred_x y pred_z)
      ]
  ;;
  
  let sub x y z =
    Fresh.one (fun valid ->
      loe y x valid
      &&& conde
            [ valid === !!false &&& (z === !!None)
            ; Fresh.one (fun z_value ->
                valid === !!true &&& (z === !!(Some z_value)) &&& add y z_value x)
            ])
  ;;
  
  let rec loe_2true x y =
    x
    === !!O
    ||| Fresh.two (fun pred_x pred_y ->
      x === !!(S pred_x) &&& (y === !!(S pred_y) &&& loe_2true pred_x pred_y))
  
  and loe_2false x y =
    Fresh.one (fun pred_x -> x === !!(S pred_x) &&& (y === !!O))
    ||| Fresh.two (fun pred_x pred_y ->
      x === !!(S pred_x) &&& (y === !!(S pred_y) &&& loe_2false pred_x pred_y))
  
  and sub_2None x y = loe_2false y x
  and sub_2Some x y constarg0 = loe_2true y x &&& add y constarg0 x
