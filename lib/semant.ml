open Ocanren_patterns

module Value = struct
  type value =
    | Var of Ident.t
    | Constr of Types.constructor_description * value list

  let rec to_string = function
    | Var v -> Printf.sprintf "%s" @@ Ident.name v
    | Constr (desc, values) ->
      values
      |> List.map to_string
      |> String.concat " "
      |> Printf.sprintf "%s (%s)" desc.cstr_name
  ;;
end

module IdentMap = struct
  include Map.Make (Ident)

  let pp f =
    iter (fun ident ls ->
      Format.fprintf f "%s = " @@ Ident.name ident;
      List.iter (fun x -> Format.fprintf f "%s " @@ Value.to_string x) ls;
      Format.fprintf f "\n")
  ;;
end

open Value

module Ctx = struct
  type ctx =
    { global : Ident.t list
    ; fresh : Ident.t list
    ; tbl : value list IdentMap.t list
    }

  let empty : ctx = { global = []; fresh = []; tbl = [ IdentMap.empty ] }
end

let get_path exp =
  let open Typedtree in
  exp.exp_desc
  |> function
  | Texp_ident (path, _, _) -> path
  | _ -> failwith "get path. fail"
;;

let get_value vars exp =
  let open Typedtree in
  let is_var ident = List.exists (Ident.same ident) vars in
  let rec loop exp =
    match exp.exp_desc with
    | Texp_construct (_, cons_desc, args) ->
      args |> List.map loop |> fun args -> Constr (cons_desc, args)
    | Texp_ident (Path.Pident ident, _, _) when is_var ident -> Var ident
    | Texp_apply (hd, args) when Ocanren_patterns.is_inj hd ->
      let arg = Assert.un_arg args in
      loop arg
    | Texp_ident _ -> failwith "Seems like dot ident in values."
    | _ -> failwith "Get value fail"
  in
  loop exp
;;

open Value

module Canren = struct
  type ('a, 'b) canren =
    | Call of Path.t * value list
    | Fresh of Ident.t list * ('a, 'b) canren
    | Unify of 'a * 'b
    | Disunify of 'a * 'b
    | Disj of ('a, 'b) canren * ('a, 'b) canren
    | Conj of ('a, 'b) canren * ('a, 'b) canren

  let rec pp f pfst psnd = function
    | Call (ident, values) ->
      Format.fprintf f "%s (" @@ Path.name ident;
      List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
      Format.fprintf f ")"
    | Fresh (vars, next) ->
      Format.fprintf f "Fresh (";
      List.iter (fun ident -> Format.fprintf f "%s " @@ Ident.name ident) vars;
      Format.fprintf f ") (";
      pp f pfst psnd next;
      Format.fprintf f ") \n"
    | Unify (fst, snd) ->
      pfst f fst;
      Format.fprintf f "===";
      psnd f snd
    | Disunify (fst, snd) ->
      Format.fprintf f "(";
      pfst f fst;
      Format.fprintf f "=/=";
      psnd f snd;
      Format.fprintf f ")"
    | Disj (fst, snd) ->
      pp f pfst psnd fst;
      Format.fprintf f " /\\ ";
      pp f pfst psnd snd
    | Conj (fst, snd) ->
      pp f pfst psnd fst;
      Format.fprintf f " & ";
      pp f pfst psnd snd
  ;;

  type dnf_ctx =
    { global : Ident.t list
    ; fresh : Ident.t list
    }

  let to_dnf exp =
    let open Typedtree in
    let get_vars { fresh; global; _ } = fresh @ global in
    let get_params exp =
      let rec loop acc exp =
        match exp.exp_desc with
        | Texp_function { param; cases = [ { c_rhs; _ } ]; _ } ->
          loop (param :: acc) c_rhs
        | Texp_function _ -> failwith "Not implemented. Texp_function in fresh travers"
        | _ -> exp, acc
      in
      loop [] exp
    in
    let rec loop (ctx : dnf_ctx) exp =
      match exp.exp_desc with
      | Texp_function _ -> failwith "Assumed no functions."
      | Texp_apply (hd_exp, args) when is_conde hd_exp ->
        let e = Assert.un_arg args in
        loop ctx e
      (* (::) list cons. assume disj *)
      | Texp_construct (_, _, args) when is_list_cons exp ->
        let fst, snd = Assert.bin args in
        let fst = loop ctx fst in
        let snd = loop ctx snd in
        Core.Option.merge fst snd ~f:(fun fst snd -> Disj (fst, snd))
      (* (|||) disj *)
      | Texp_apply (hd_exp, args) when is_disj hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        let fexp = loop ctx fexp in
        let sexp = loop ctx sexp in
        Core.Option.merge fexp sexp ~f:(fun fexp sexp -> Disj (fexp, sexp))
        (* (&&&) conj *)
      | Texp_apply (hd_exp, args) when is_conj hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        let fexp = loop ctx fexp in
        let sexp = loop ctx sexp in
        Core.Option.merge fexp sexp ~f:(fun fexp sexp -> Conj (fexp, sexp))
      (* === *)
      | Texp_apply (hd_exp, args) when is_unify hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        let fst = get_value (get_vars ctx) fexp in
        let snd = get_value (get_vars ctx) sexp in
        Option.some (Unify (fst, snd))
      (* =/= *)
      | Texp_apply (hd_exp, args) when is_nunify hd_exp ->
        let fexp, sexp = Assert.bin_args args in
        let fst = get_value (get_vars ctx) fexp in
        let snd = get_value (get_vars ctx) sexp in
        Option.some (Disunify (fst, snd))
        (* fresh *)
      | Texp_apply (hd, args) when is_fresh hd ->
        (* TODO if fresh var unif with variant *)
        let e = Assert.un_arg args in
        let exp, new_fresh = get_params e in
        let ctx = { ctx with fresh = new_fresh @ ctx.fresh } in
        Option.map (fun x -> Fresh (new_fresh, x)) @@ loop ctx exp
      | Texp_apply (hd_exp, args) ->
        let args = Assert.args args in
        let args = List.map (get_value (get_vars ctx)) args in
        let ident = get_path hd_exp in
        Call (ident, args) |> Option.some
      (* paramter -> variant *)
      | _ -> None (* hack to hold :: in conde. rework *)
    in
    let body, global = get_params exp in
    let ctx = { global; fresh = [] } in
    loop ctx body
  ;;

  let rec remove_cons_unify (c : (value, value) canren) : (Ident.t, value) canren =
    let map fst snd =
      match fst, snd with
      | Var v, x | x, Var v -> v, x
      | Constr _, Constr _ -> failwith "Not implemeted"
    in
    match c with
    | Call (hd, args) -> Call (hd, args)
    | Fresh (args, next) -> Fresh (args, remove_cons_unify next)
    | Disj (left, right) -> Disj (remove_cons_unify left, remove_cons_unify right)
    | Conj (left, right) -> Conj (remove_cons_unify left, remove_cons_unify right)
    | Unify (left, right) ->
      let fst, snd = map left right in
      Unify (fst, snd)
    | Disunify (left, right) ->
      let fst, snd = map left right in
      Disunify (fst, snd)
  ;;

  let reduce_by_const const vars (is_par : Ident.t -> bool) (c : (Ident.t, value) canren) =
    let map_unify left right =
      (* par === Cons (var1, var2, ...) -> new_var1 === var1 &&& new_var2 === var2 &&& ...*)
      match left, right with
      | ident, Constr (desc, values) when is_par ident && const#by_desc desc ->
        (match values, vars with
         | [], [] -> Sresult.Empty
         | value_hd :: value_tl, var_hd :: var_tl ->
           List.map2 (fun value var -> Unify (var, value)) value_tl var_tl
           |> List.fold_left (fun x y -> Conj (x, y)) (Unify (var_hd, value_hd))
           |> Sresult.expr
         | _ -> assert false)
      | ident, Constr (desc, _) when is_par ident && (not @@ const#by_desc desc) ->
        Sresult.ReduceConj
        (* some_var === par -> some_var === Cons (new_var0, new_var1, ..)*)
      | ident, Var v when is_par ident && (not @@ is_par v) ->
        let vars = List.map (fun v -> Var v) vars in
        Sresult.expr @@ Unify (v, Constr (const#desc, vars))
      | ident, Var v when is_par v && (not @@ is_par ident) ->
        let vars = List.map (fun v -> Var v) vars in
        Sresult.expr @@ Unify (ident, Constr (const#desc, vars))
      | _ -> Sresult.expr @@ Unify (left, right)
    in
    let map_disunify left right =
      match left, right with
      | ident, Constr (desc, _) when is_par ident && const#by_desc desc ->
        Sresult.ReduceConj
      | _, Constr (desc, values) -> Sresult.expr @@ (left, Constr (desc, values))
      | ident, Var v -> Sresult.expr @@ (ident, Var v)
    in
    let rec loop = function
      | Call (hd, args) -> Sresult.expr @@ Call (hd, args)
      | Fresh (args, next) ->
        let next = loop next in
        Sresult.map ~f:(fun x -> Fresh (args, x)) next
      | Disj (left, right) ->
        let left = loop left in
        let right = loop right in
        Sresult.reduce_disj left right (fun fst snd -> Disj (fst, snd))
      | Conj (left, right) ->
        let left = loop left in
        let right = loop right in
        Sresult.reduce_conj left right (fun fst snd -> Conj (fst, snd))
      | Unify (left, right) -> map_unify left right
      | Disunify (left, right) ->
        Sresult.map ~f:(fun (fst, snd) -> Disunify (fst, snd)) (map_disunify left right)
    in
    loop c
  ;;
end

let process par const exp =
  let mk = Ident.create_local in
  let vars = List.init const#arity (fun i -> mk @@ Printf.sprintf "new_var%d" i) in
  let result =
    Canren.to_dnf exp
    |> Option.get
    |> Canren.remove_cons_unify
    |> Canren.reduce_by_const const vars par#by_ident
    |> Sresult.get
  in
  let pfst f ident = Format.fprintf f "%s" @@ Ident.name ident in
  let psnd f value = Format.fprintf f "%s" @@ Value.to_string value in
  Format.fprintf Format.std_formatter "\n%!";
  Canren.pp Format.std_formatter pfst psnd result
;;
