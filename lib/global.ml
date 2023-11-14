open Dnf
open Value

type error = Unnamed of string

exception Error of error

let error e = raise @@ Error e

type fun_hole = (string * Value.t list) option
type spec_info = (int * Types.constructor_description) list

type fun_info =
  { fname : Ident.t
  ; consts : spec_info
  }

type hole_info =
  { hfinfo : fun_info
  ; href : fun_hole ref
  ; hargs : Value.t list
  }

type ('a, 'b) item =
  | Node of 'a
  | Hole of 'b option ref

type ('a, 'b) conj = ('a, 'b) item list
type ('a, 'b) dnf = ('a, 'b) conj list

type ('a, 'b) t =
  { res_info : fun_info
  ; res_dnf : ('a, 'b) dnf
  ; res_deps : hole_info list
  ; res_globals : Ident.t list
  }

let equal fst snd =
  let consts_same =
    List.length fst.consts = List.length snd.consts
    && List.for_all2
         (fun (fi, fc) (si, sc) ->
           Int.equal fi si && Types.(equal_tag fc.cstr_tag sc.cstr_tag))
         fst.consts
         snd.consts
  in
  Ident.same fst.fname snd.fname && consts_same
;;

let fetch is_par conj =
  let get = function
    | DCall (f_path, args) as call ->
      let consts =
        args
        |> List.mapi (fun index -> function
          | Constr (desc, _) -> Some (index, desc)
          | Var _ -> None)
        |> List.filter_map (fun x -> x)
      in
      if Core.List.is_empty consts || (is_par @@ Path.head f_path)
      then Node call, None
      else (
        let hargs =
          List.concat_map
            (function
             | Constr (_, values) -> values
             | x -> [ x ])
            args
        in
        let href = ref None in
        let hfinfo = { fname = Path.head f_path; consts } in
        Hole href, Some { href; hfinfo; hargs })
    | x -> Node x, None
  in
  let fdnf, funs = List.map get conj |> Core.List.unzip in
  let funs = List.filter_map (fun x -> x) funs in
  fdnf, funs
;;

let process globals dnf : _ dnf * hole_info list =
  let is_par ident = List.exists (Ident.same ident) globals in
  let dnf, funs = List.map (fetch is_par) dnf |> Core.List.unzip in
  let funs = List.concat funs in
  dnf, funs
;;

let get a = !a |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Hole")

let pp f =
  let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
  let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
  List.iter (fun l ->
    Format.printf "\n new disj \n";
    List.iter
      (fun x ->
        Format.printf " && ";
        match x with
        | Node item -> Dnf.pp f pfst psnd item
        | Hole a -> get a |> Dnf.pp f pfst psnd)
      l)
;;

let to_dnf dnf =
  let hole_to_dnf (fst, values) = DCall (Path.Pident (Ident.create_local fst), values) in
  let item = function
    | Node x -> x
    | Hole x -> get x |> hole_to_dnf
  in
  List.map (fun cnj -> List.map item cnj) dnf
;;

let create_name source_info =
  let postfix =
    source_info.consts
    |> List.map (fun (_, (x : Types.constructor_description)) -> x.cstr_name)
    |> String.concat "_"
  in
  Ident.name source_info.fname ^ "_" ^ postfix
;;

let step get create_info { fname; consts } =
  let globals, canren = get fname in
  let freshs = Canren.get_declared_fresh_vars canren in
  let info = create_info globals consts in
  let dnf = Dnf.of_canren canren in
  let Local.{ dnf; globals } = Local.run ~info ~freshs ~globals ~dnf in
  let res_dnf, res_deps = dnf |> process globals in
  { res_dnf; res_deps; res_info = { fname; consts }; res_globals = globals }
;;

let closer step source =
  let set_call_self { href; hfinfo; hargs; _ } =
    href := Some (create_name hfinfo, hargs)
  in
  let filter acc deps =
    deps
    |> List.filter_map (fun (hole : hole_info) ->
      List.find_opt (fun item -> equal hole.hfinfo item.res_info) acc
      |> function
      | Some _ ->
        set_call_self hole;
        None
      | _ -> Some hole)
  in
  let rec loop acc deps =
    let deps = filter acc deps in
    let front = List.map (fun hole -> step hole.hfinfo) deps in
    let deps = List.concat_map (fun res -> res.res_deps) front @ deps |> filter acc in
    let acc = front @ acc in
    if Core.List.is_empty deps then acc else loop acc deps
  in
  let init = List.map step source in
  let deps = init |> List.concat_map (fun { res_deps; _ } -> res_deps) |> filter init in
  loop init deps
;;

let run (source : fun_info list) get create_info =
  let step = step get create_info in
  closer step source
;;
