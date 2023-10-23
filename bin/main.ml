open SpecialKanren

let anon_fun = Config.set_path
let message = "Path, parameters, etc. TODO()"

let options =
  let open Config in
  [ "-I", Arg.String add_dir, "<dir>  Add <dir> to the list of include directories"
  ; "-open", Arg.String add_open, "<module>  Opens the module <module> before typing"
  ; "-par", Arg.String set_param, "<parameter> Set output parameter name to <file>"
  ; "-fname", Arg.String set_fname, "<fname> Set function name to <fname>"
  ; "-v", Arg.Unit verbose_on, "Set verbose on"
  ]
;;

let () = Arg.parse options anon_fun message

let read_ml_file input_name =
  (* add config dirs and opens TODO()*)
  Clflags.include_dirs := List.append (Utils.get_std_lib_pathes ()) !Clflags.include_dirs;
  Clflags.recursive_types := true;
  Clflags.open_modules := List.append Utils.std_opens !Clflags.open_modules;
  Compmisc.init_path ();
  let open Compenv in
  let outputprefix = output_prefix input_name in
  let modulename = module_of_filename input_name outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env () in
  let { Typedtree.structure = s; _ } =
    Pparse.parse_implementation ~tool_name:Config.tool_name input_name
    |> Typemod.type_implementation input_name outputprefix modulename env
  in
  s
;;

let () =
  let path = Config.path () in
  let par = Config.param () in
  let fname = Config.fname () in
  if Config.verbose () then Printf.printf "path: %s\npar: %s\nfname: %s%!" path par fname;
  let str = read_ml_file path in
  let parp = Predicate.par_of_string par in
  let funp = Predicate.fun_of_string fname in
  SpecialKanren.Translator.translate funp parp str
;;
