open Dnf
open Value

type error = Unnamed of string

exception Error of error

let error e = raise @@ Error e

type fun_hole = (string * value list) option ref
type spec_info = (int * Types.constructor_description) list

type fun_info =
  { fname : Ident.t
  ; consts : spec_info
  }

type hole_info =
  { hfinfo : fun_info
  ; href : fun_hole
  ; hargs : value list
  }

type ('a, 'b) item =
  | FUnify of 'a * 'b
  | FDisunify of 'a * 'b
  | FFresh of Ident.t list
  | FCallHole of fun_hole
  | FCall of (Path.t * value list)

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

let fetch conj =
  let get = function
    | DCall (f_path, args) ->
      let consts =
        args
        |> List.mapi (fun index -> function
          | Constr (desc, _) -> Some (index, desc)
          | Var _ -> None)
        |> List.filter_map (fun x -> x)
      in
      if Core.List.is_empty consts
      then FCall (f_path, args), None
      else (
        let hargs =
          List.concat_map
            (function
             | Constr (_, values) -> values
             | x -> [ x ])
            args
        in
        let href = ref None in
        let fname = Path.head f_path in
        let hfinfo = { fname; consts } in
        FCallHole href, Some { href; hfinfo; hargs })
    | DUnify (left, right) -> FUnify (left, right), None
    | DDisunify (left, right) -> FDisunify (left, right), None
    | DFresh vars -> FFresh vars, None
  in
  let fdnf, funs = List.map get conj |> Core.List.unzip in
  let funs = List.filter_map (fun x -> x) funs in
  fdnf, funs
;;

let process dnf : _ dnf * hole_info list =
  let dnf, funs = List.map fetch dnf |> Core.List.unzip in
  let funs = List.concat funs in
  dnf, funs
;;

let pp f =
  let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
  let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
  List.iter (fun l ->
    Format.printf "\n new disj \n";
    List.iter
      (fun x ->
        Format.printf " && ";
        match x with
        | FUnify (fst, snd) ->
          pfst f fst;
          Format.fprintf f "===";
          psnd f snd
        | FDisunify (fst, snd) ->
          Format.fprintf f "(";
          pfst f fst;
          Format.fprintf f "=/=";
          psnd f snd
        | FCall (ident, values) ->
          Format.fprintf f "%s (" @@ Path.name ident;
          List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
          Format.fprintf f ")"
        | FCallHole rf ->
          (match !rf with
           | Some (name, values) ->
             Format.fprintf f "%s (" name;
             List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
             Format.fprintf f ")"
           | None -> Format.printf "HOLE")
        | FFresh freshs ->
          Format.fprintf f "Fresh (";
          List.iter (fun i -> Format.fprintf f "%s " @@ Ident.name i) freshs;
          Format.fprintf f ")")
      l)
;;

let to_dnf dnf =
  let item = function
    | FUnify (left, right) -> DUnify (left, right)
    | FDisunify (left, right) -> DDisunify (left, right)
    | FFresh fresh -> DFresh fresh
    | FCallHole hole ->
      (match !hole with
       (* TODO *)
       | Some (name, values) -> DCall (Path.Pident (Ident.create_local name), values)
       | None -> error @@ Unnamed "Hole in resolve representation")
    | FCall (path, values) -> DCall (path, values)
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
