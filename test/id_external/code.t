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
  
  let fve_id fst snd = fst === snd
  let for_id fst snd = fst =/= snd
  
  let snd_id flag =
    conde
      [ flag === !!true &&& for_id flag !!false; flag === !!false &&& for_id flag !!true ]
  ;;
  
  let thd_id flag =
    conde
      [ flag === !!true &&& for_id flag !!true; flag === !!false &&& fve_id flag !!false ]
  ;;
  
  let id flag = conde [ flag === !!true &&& snd_id flag; flag === !!false &&& thd_id flag ]
  
  let rec for_id_0true_1false = failwith "Reduced"
  and fve_id_0false_1false = failwith "Reduced"
  and thd_id_0false = fve_id_0false_1false
  and snd_id_0true = for_id_0true_1false
  and id_0true = snd_id_0true
  and id_0false = thd_id_0false
