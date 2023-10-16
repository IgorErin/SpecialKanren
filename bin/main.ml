open SpecialKanren

let anon_fun = SpecialKanren.Config.set_path
let message = ""

let options =
  let open SpecialKanren.Config in
  [ "-par", Arg.String set_param, "<parameter> Set output parameter name to <file>"
  ; "-var", Arg.String set_variant, "<variant> Set variant name to <variant> "
  ]
;;

let config = SpecialKanren.Config.default
let () = Arg.parse options anon_fun message

(* TODO *)
let () = config.verbose <- true

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
  if Config.verbose ()
  then Printf.printf "path: %s\npar: %s\nvariant: %s\n%!" path par variant;
  let cmt = read_cmt_impl path in
  SpecialKanren.Translator.translate par variant cmt
;;
