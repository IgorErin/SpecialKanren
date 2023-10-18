open SpecialKanren

(* TODO *)
let anon_fun = SpecialKanren.Config.set_path
let message = ""

let options =
  let open SpecialKanren.Config in
  [ "-par", Arg.String set_param, "<parameter> Set output parameter name to <file>"
  ; "-var", Arg.String set_variant, "<variant> Set variant name to <variant> "
  ; "-fname", Arg.String set_fname, "<fname> Set function name to <fname>"
  ; "-v", Arg.Unit verbose_on, "Set verbose on"
  ]
;;

let () = Arg.parse options anon_fun message

let read_cmt_impl path =
  if Stdlib.Sys.file_exists path
  then (
    let infos = Cmt_format.read_cmt path in
    infos.Cmt_format.cmt_annots
    |> function
    | Cmt_format.Implementation cmt ->
      if Config.verbose () then Printf.printf "Reading cmt file %s \n%!" path;
      cmt
    | _ -> failwith "Implementation expected")
  else failwith "File does not exist"
;;

let create_par_predicate name =
  object
    method exp =
      let open Patterns in
      let open Ocanren_patterns in
      parse_bool Gen.(exp_by_texp_ident [ str name ])

    method ident x = String.equal name @@ Ident.name x
  end
;;

(* full path for now *)
let create_var_predicate path =
  let open Patterns in
  let open Ocanren_patterns in
  let spec_variant =
    path
    |> String.split_on_char '.'
    |> List.map Patterns.Gen.str
    |> fun x -> exp_by_texp_ident x
  in
  object
    method exp = parse_bool spec_variant
  end
;;

let create_fun_predicate name =
  let open Patterns in
  let open Gen in
  object
    method ident x = String.equal name @@ Ident.name x

    method pat =
      let id = Patterns.ident name in
      let pat_desc = Pattern_desc.tpat_var id drop in
      let pat_data = Patterns.pattern_data pat_desc drop drop drop drop drop in
      parse_bool pat_data
  end
;;

let () =
  let path = SpecialKanren.Config.path () in
  let par = SpecialKanren.Config.param () in
  let var = SpecialKanren.Config.variant () in
  let fname = SpecialKanren.Config.fname () in
  if Config.verbose ()
  then Printf.printf "path: %s\npar: %s\nvariant: %s\nfname: %s%!" path par var fname;
  let cmt = read_cmt_impl path in
  let spec_var = create_par_predicate par in
  let spec_variant = create_var_predicate var in
  let fun_predicate = create_fun_predicate fname in
  SpecialKanren.Translator.translate spec_var spec_variant fun_predicate cmt
;;
