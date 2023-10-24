let std_lib_names = [ "OCanren" ]
let std_opens = [ "OCanren" ]

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
