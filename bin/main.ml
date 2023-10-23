open SpecialKanren

(* TODO *)
let anon_fun = SpecialKanren.Config.set_path
let message = ""

let options =
  let open SpecialKanren.Config in
  [ "-par", Arg.String set_param, "<parameter> Set output parameter name to <file>"
  ; "-fname", Arg.String set_fname, "<fname> Set function name to <fname>"
  ; "-v", Arg.Unit verbose_on, "Set verbose on"
  ]
;;

let () = Arg.parse options anon_fun message
let tool_name = "SpecialKanren"
let std_lib_names = [ "OCanren" ]

let get_std_lib_pathes () =
  List.filter_map
    (fun name ->
      let open Findlib in
      try Some (package_directory name) with
      | No_such_package (lib_name, reason) ->
        Printf.eprintf
          "No such std package '%s'%s\n"
          lib_name
          (if reason <> "" then Printf.sprintf ", reason: %s" reason else ".");
        None)
    std_lib_names
;;

let read_ml_file input_name =
  Clflags.include_dirs := List.append (get_std_lib_pathes ()) !Clflags.include_dirs;
  Clflags.recursive_types := true;
  Clflags.open_modules := List.append [ "OCanren" ] !Clflags.open_modules;
  Compmisc.init_path ();
  let open Compenv in
  let outputprefix = output_prefix input_name in
  let modulename = module_of_filename input_name outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env () in
  let { Typedtree.structure = s; _ } =
    Pparse.parse_implementation ~tool_name input_name
    |> Typemod.type_implementation input_name outputprefix modulename env
  in
  s
;;

let () =
  let path = SpecialKanren.Config.path () in
  let par = SpecialKanren.Config.param () in
  let fname = SpecialKanren.Config.fname () in
  if Config.verbose () then Printf.printf "path: %s\npar: %s\nfname: %s%!" path par fname;
  let str = read_ml_file path in
  let parp = Predicate.par_of_string par in
  let funp = Predicate.fun_of_string fname in
  SpecialKanren.Translator.translate funp parp str
;;
