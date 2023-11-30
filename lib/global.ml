type value_dnf = (Value.t, Value.t) Dnf.dnf
type value_dnf_item = (Value.t, Value.t) Dnf.item
type fcall = Path.t * Value.t list
type hole_ref = fcall option ref

type item =
  | Node of value_dnf_item
  | Hole of (fcall * hole_ref)

type partial = item list list

let trans sfunc =
  let is_par x =
    let id = Path.head x in
    List.exists (Ident.same id) sfunc.Fun.func.params
  in
  let trans body =
    let open Dnf in
    let has_const = List.exists (fun x -> x |> Value.constr_get_opt |> Option.is_some) in
    let loop = function
      | DCall (name, args) when (not @@ is_par name) && has_const args ->
        let r = ref None in
        let call = name, args in
        Hole (call, r)
      | x -> Node x
    in
    let body = body |> List.map @@ List.map loop in
    body
  in
  let sfunc = Fun.map ~f:trans sfunc in
  sfunc
;;

let to_dnf dnf =
  let get a = !a |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Hole") in
  let open Dnf in
  let hole_to_dnf (name, values) = DCall (name, values) in
  let item = function
    | Node x -> x
    | Hole (_, h) -> get h |> hole_to_dnf
  in
  List.map (fun cnj -> List.map item cnj) dnf
;;

let create_name Fun.{ func; spec } =
  let module P = Predicate in
  let postfix =
    spec
    |> List.map (fun Spec.{ var; par } ->
      (Int.to_string @@ P.Par.Id.num par) ^ P.Var.name var)
    |> String.concat "_"
  in
  Ident.name func.name ^ "_" ^ postfix
;;

module Deps = struct
  type t =
    { func : Canren.canren Fun.spec
    ; href : hole_ref
    ; args : Value.t list
    }

  let unwrap_arg args =
    let map =
      let open Value in
      function
      | Var v -> Var v |> Core.List.return
      | Constr (_, args) -> args
    in
    List.concat_map map args
  ;;

  let collect ~src =
    let create_spec args params =
      assert (List.length args = List.length params);
      List.combine args params
      |> List.mapi (fun id (arg, par) -> id, arg, par)
      |> List.map (fun (num, arg, par) ->
        arg
        |> Value.constr_get_opt
        |> Option.map (fun (desc, _) ->
          let par = Predicate.Par.Id.of_ident ~id:par ~num in
          let var = Predicate.Var.create ~cur:desc in
          Spec.{ par; var }))
      |> List.filter_map (fun x -> x)
    in
    let fetch = function
      | Hole ((path, args), r) ->
        let func =
          let name = Path.head path in
          Outer.find ~src ~name
        in
        let spec = create_spec args func.params in
        let args = unwrap_arg args in
        let func = Fun.{ func; spec } in
        (* TODO rework
           we set the name and args before for simplisity *)
        let name = create_name func |> Ident.create_local |> fun x -> Path.Pident x in
        let () = r := Some (name, args) in
        Some func
      | _ -> None
    in
    List.concat_map @@ List.filter_map fetch
  ;;

  let get ~src ~body = body |> collect ~src
end

let step ~func =
  let fun_to_spec = Fun.canren_to_dnf func in
  let func = Local.run ~info:fun_to_spec |> trans in
  func
;;

module Closer : sig
  val run : src:Outer.t -> tgt:Canren.canren Fun.spec list -> partial Fun.spec list
end = struct
  let run ~src ~tgt =
    let exists_in acc dep = List.exists (Fun.equal dep) acc in
    let filter acc deps = List.filter (fun dep -> not @@ exists_in acc dep) deps in
    let compile acc deps =
      deps
      |> List.fold_left
           (fun ((compiled, deps) as default) next ->
             if exists_in compiled next
             then default
             else (
               let next = step ~func:next in
               let next_deps = Deps.get ~src ~body:next.func.body |> filter compiled in
               next :: compiled, next_deps @ deps))
           (acc, deps)
      |> fun (acc, deps) -> acc, filter acc deps
    in
    let rec loop acc deps =
      let acc, deps = compile acc deps in
      if Base.List.is_empty deps then acc else loop acc deps
    in
    loop [] tgt
  ;;
end

let get a = !a |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Hole")

let run ~(src : Outer.t) ~(targets : Canren.canren Fun.spec list) =
  Closer.run ~src ~tgt:targets
  |> List.map (Fun.map ~f:to_dnf)
  |> List.map (fun f ->
    let name = (fun x -> x |> create_name |> Ident.create_local) f in
    Fun.chname ~f ~name)
;;
