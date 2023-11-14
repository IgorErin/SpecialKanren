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
  ; "-cmt", Arg.Unit cmt, "Set file format"
  ; "-ml", Arg.Unit ml, "Set file format"
  ; "-o", Arg.String set_out, "Set output file name"
  ]
;;

let read_cmt path =
  let open Tast_mapper in
  let map_env s =
    let mapper = { default with env = (fun _ env -> Envaux.env_of_only_summary env) } in
    mapper.structure mapper s
  in
  let infos = Cmt_format.read_cmt path in
  infos.Cmt_format.cmt_annots
  |> function
  | Cmt_format.Implementation cmt ->
    Load_path.init infos.cmt_loadpath;
    map_env cmt
  | _ -> failwith "Implementation expected"
;;

let read_ml input_name =
  Clflags.include_dirs
    := List.concat
         [ Utils.get_std_lib_pathes (); Config.include_dirs (); !Clflags.include_dirs ];
  Clflags.open_modules
    := List.concat [ Utils.std_opens; Config.opens (); !Clflags.open_modules ];
  Compmisc.init_path ();
  let outputprefix = Compenv.output_prefix input_name in
  let modulename = Compenv.module_of_filename input_name outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env () in
  let { Typedtree.structure = s; _ } =
    Pparse.parse_implementation ~tool_name:Config.tool_name input_name
    |> Typemod.type_implementation input_name outputprefix modulename env
  in
  s
;;

(* init *)
let () =
  let () = Arg.parse options anon_fun message in
  let () = Clflags.recursive_types := true in
  ()
;;

let () =
  let path = Config.path () in
  let par = Config.param () in
  let fname = Config.fname () in
  let out = Config.get_out () in
  if Config.verbose () then Printf.printf "path: %s\npar: %s\nfname: %s%!" path par fname;
  let parp = Predicate.par_of_string par in
  let funp = Predicate.fun_of_string fname in
  let ff = Config.ff () in
  let items =
    match ff with
    | Ml ->
      let str = read_ml path in
      SpecialKanren.Outer.translate funp parp str
    | Cmt ->
      let str = read_cmt path in
      SpecialKanren.Outer.translate funp parp str
  in
  match out with
  | Some output_name ->
    let ch = open_out output_name in
    let fmt = Format.formatter_of_out_channel ch in
    Format.pp_set_margin fmt 180;
    Pprintast.structure fmt items;
    Format.fprintf fmt "%!"
  | None -> Pprintast.structure Format.std_formatter items
;;
