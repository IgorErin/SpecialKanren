open SpecialKanren

(* TODO *)
let anon_fun = SpecialKanren.Config.set_path
let message = ""

let options =
  let open SpecialKanren.Config in
  [ "-par", Arg.String set_param, "<parameter> Set output parameter name to <file>"
  ; "-var", Arg.String set_variant, "<variant> Set variant name to <variant> "
  ; "-fname", Arg.String set_fname, "<fname> Set function name to <fname>"
  ; "-v", Arg.Unit verbose_on, "Set verbose on"]
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

let () =
  let path = SpecialKanren.Config.path () in
  let par = SpecialKanren.Config.param () in
  let variant = SpecialKanren.Config.variant () in
  let fname = SpecialKanren.Config.fname () in 
  if Config.verbose ()
  then Printf.printf "path: %s\npar: %s\nvariant: %s\nfname: %s%!" path par variant fname;
  let cmt = read_cmt_impl path in
  let spec_var =
    object
      method exp =
        let open Patterns in
        let open Ocanren_patterns in
        parse_bool Gen.(exp_by_texp_ident [ str par ])

      method ident x = String.equal par @@ Ident.name x
    end
  in
  let spec_variant =
    let open Patterns in
    let open Ocanren_patterns in
    let spec_variant =
      variant 
      |> String.split_on_char '.' 
      |> List.map Patterns.Gen.str
      |> fun x -> exp_by_texp_ident x
    in
    object
      method exp = parse_bool spec_variant
    end
  in
  SpecialKanren.Translator.translate spec_var spec_variant cmt
;;
