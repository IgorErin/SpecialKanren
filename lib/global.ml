type value_dnf = (Value.t, Value.t) Dnf.dnf
type value_dnf_item = (Value.t, Value.t) Dnf.item
type fcall = Path.t * Value.t list
type hole_ref = fcall option ref

type node =
  | Node of value_dnf_item
  | Hole of (fcall * hole_ref)

type raw_dnf = node list list
type gdnf = node list list

let trans sfunc =
  let is_par x =
    let id = Path.head x in
    List.exists (Ident.same id) sfunc.Fun.func.params
  in
  let trans body =
    let open Dnf in
    let has_const = List.exists (fun x -> x |> Value.constr_get_opt |> Option.is_some ) in 
    let loop = function
      | DCall (name, args) when not @@ is_par name && has_const args ->
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
    { func : Canren.canren Fun.to_spec
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
        let name = Path.head path in
        let func = Outer.find ~src ~name in
        let spec = create_spec args func.params in
        let args = unwrap_arg args in
        let func = Fun.{ func; spec } in
        Some { func; href = r; args }
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

let closer ~src ~tgt =
  let set_call_self Deps.{ func; href; args } =
    let name = create_name func in
    let path = Path.Pident (Ident.create_local name) in
    href := Some (path, args)
  in
  let try_set acc dep =
    List.find_opt (fun item -> Fun.equal dep.Deps.func item) acc
    |> function
    | Some _ ->
      set_call_self dep;
      None
    | _ -> Some dep
  in
  let filter acc deps = deps |> List.filter_map (fun hole -> try_set acc hole) in
  let step x = step ~func:x in
  let rec loop acc deps =
    let deps = filter acc deps in
    let acc, deps =
      List.fold_left
        (fun (items, deps) new_dep ->
          let new_items =
            new_dep
            |> try_set items
            |> Option.to_list
            |> List.map (fun h -> step h.Deps.func)
          in
          let new_deps =
            new_items |> List.concat_map (fun x -> Deps.get ~src ~body:x.Fun.func.body)
          in
          new_items @ items, new_deps @ deps)
        (acc, deps)
        deps
    in
    let deps = filter acc deps in
    if Core.List.is_empty deps then acc else loop acc deps
  in
  let init = List.map step tgt in
  let deps = init |> List.concat_map (fun res -> Deps.get ~src ~body:res.Fun.func.body) in
  loop init deps
;;

let get a = !a |> Core.Option.value_or_thunk ~default:(fun () -> failwith "Hole")

let run ~(src : Outer.t) ~(targets : Canren.canren Fun.to_spec list) =
  closer ~src ~tgt:targets
  |> List.map (Fun.map ~f:to_dnf)
  |> List.map (fun f ->
    let name = (fun x -> x |> create_name |> Ident.create_local) f in
    Fun.chname ~f ~name)
;;
