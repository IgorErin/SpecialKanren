open SpecialKanren

let anon_fun = SpecialKanren.Config.set_path
let message = ""
let options = []
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
  match path with
  | Some path ->
    let cmt = read_cmt_impl path in
    SpecialKanren.Translator.translate cmt
  | None -> failwith "No input file specified"
;;
