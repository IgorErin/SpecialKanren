open Ocanren_patterns

type value =
  | Var of Ident.t
  | Constr of Types.constructor_description * value list

module IdentMap = Map.Make (Ident)

module Ctx = struct
  type ctx =
    { global : Ident.t list
    ; fresh : Ident.t list
    ; tbl : value list IdentMap.t list
    }

  let empty : ctx = { global = []; fresh = []; tbl = [ IdentMap.empty ] }
end

let merge_disj fst snd =
  let open Ctx in
  (* assume global vars unchanged during body travers *)
  { fst with fresh = fst.fresh @ snd.fresh; tbl = fst.tbl @ snd.tbl }
;;

let merge_conj fst snd =
  let open Ctx in
  let fresh = fst.fresh @ snd.fresh in
  let merge fst snd =
    IdentMap.merge
      (fun _ fst snd ->
        match fst, snd with
        | Some fst, Some snd -> Some (fst @ snd)
        | Some x, None | None, Some x -> Some x
        | _ -> None)
      fst
      snd
  in
  let tbl =
    fst.tbl
    |> List.map (fun fst -> List.map (fun snd -> merge fst snd) snd.tbl)
    |> List.concat
  in
  { fst with fresh; tbl }
;;

let get_value ctx exp =
  let open Typedtree in
  let open Ctx in
  let is_var ident =
    let exist = List.exists (Ident.same ident) in
    exist ctx.global || exist ctx.fresh
  in
  let rec loop exp =
    match exp.exp_desc with
    | Texp_construct (_, cons_desc, args) ->
      args
      |> List.map loop
      |> Core.Option.all
      |> Option.map (fun args -> Constr (cons_desc, args))
    | Texp_ident (Path.Pident ident, _, _) when is_var ident -> Some (Var ident)
    | Texp_apply (hd, args) when Ocanren_patterns.is_inj hd ->
      let arg = Assert.un_arg args in
      loop arg
    | _ -> None (* TODO *)
  in
  loop exp
;;

let add ident value ls =
  let get_current map =
    IdentMap.find_opt ident map
    |> Option.to_list
    (* joit list result [] -> [] but [some_list] -> some_list *)
    |> List.concat
  in
  List.map
    (fun tbl ->
      let current = get_current tbl in
      IdentMap.add ident (value :: current) tbl)
    ls
;;

(* TODO cycl check *)
let rec unify ctx fst_val snd_val =
  let open Ctx in
  let tbl = ctx.tbl in
  let back tbl = { ctx with tbl } in
  match fst_val, snd_val with
  | Var fident, Var sident -> add fident snd_val tbl |> add sident fst_val |> back
  | (Constr _ as cons), Var ident | Var ident, (Constr _ as cons) ->
    add ident cons tbl |> back
  | Constr (fst_desc, fst_list), Constr (snd_desc, snd_list) ->
    if Types.may_equal_constr fst_desc snd_desc
    then
      List.combine fst_list snd_list
      |> List.fold_left (fun ctx (fst, snd) -> unify ctx fst snd) ctx
    else ctx
;;

let get_var_info exp =
  let open Typedtree in
  let open Ctx in
  let get_params exp =
    let rec loop acc exp =
      match exp.exp_desc with
      | Texp_function { param; cases = [ { c_rhs; _ } ]; _ } -> loop (param :: acc) c_rhs
      | Texp_function _ -> failwith "Not implemented. Texp_function in fresh travers"
      | _ -> exp, acc
    in
    loop [] exp
  in
  let rec loop (ctx : ctx) exp =
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
      Core.Option.merge fst snd ~f:(fun fst snd -> merge_disj fst snd)
    (* (|||) disj *)
    | Texp_apply (hd_exp, args) when is_disj hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fexp = loop ctx fexp in
      let sexp = loop ctx sexp in
      Core.Option.merge fexp sexp ~f:(fun fexp sexp -> merge_disj fexp sexp)
      (* (&&&) conj *)
    | Texp_apply (hd_exp, args) when is_conj hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fexp = loop ctx fexp in
      let sexp = loop ctx sexp in
      Core.Option.merge fexp sexp ~f:(fun fexp sexp -> merge_conj fexp sexp)
    (* === *)
    | Texp_apply (hd_exp, args) when is_unify hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fst_value = get_value ctx fexp in
      let snd_value = get_value ctx sexp in
      Core.Option.both fst_value snd_value
      |> Core.Option.map ~f:(fun (fst, snd) -> unify ctx fst snd)
    (* =/= *)
    (* TODO disunify *)
    (* | Texp_apply (hd_exp, args) when is_nunify hd_exp ->
       let fexp, sexp = Assert.bin_args args in
       process_unify (fun _ -> true) fexp sexp
       |> List.map (fun (ident, value) -> (ident, Not value)) *)
    (* fresh *)
    | Texp_apply (hd, args) when is_fresh hd ->
      (* TODO if fresh var unif with variant *)
      let e = Assert.un_arg args in
      let exp, new_fresh = get_params e in
      let ctx = { ctx with fresh = new_fresh @ ctx.fresh } in
      loop ctx exp
    (* paramter -> variant *)
    | _ -> None
  in
  let body, global = get_params exp in
  let ctx = { Ctx.empty with global } in
  loop ctx body
;;

let get_cons ident (tbl : value list IdentMap.t) =
  let rec app_to_var x =
    IdentMap.find_opt x tbl
    |> Option.to_list
    |> List.concat
    |> List.map loop
    |> List.concat
  and loop = function
    | Constr (desc, _) -> [ desc ]
    | Var v -> app_to_var v
  in
  app_to_var ident
;;

let reduce tbl =
  let reduce = function
    | [ x ] -> x
    | _ -> failwith "Reduce.Not implemeted"
  in
  IdentMap.to_seq tbl
  |> Seq.fold_left
       (fun acc (ident, values) -> IdentMap.add ident (reduce values) acc)
       IdentMap.empty
;;

let merge_tbl fst snd =
  IdentMap.merge
    (fun _ fst snd ->
      match fst, snd with
      | Some fst, Some _ -> Some fst (* TODO*)
      | Some x, None | None, Some x -> Some x
      | None, None -> None)
    fst
    snd
;;

type answer = Delete

let process varp parp exp =
  let open Ctx in
  (* clean path where spec param unifyed with another constructor *)
  get_var_info exp
  |> (function
       | Some x -> x
       | None -> failwith "Unify faild")
  |> fun x ->
  x.tbl
  |> List.filter_map (fun tbl ->
    let cons = get_cons parp#ident tbl in
    let has_another = List.exists varp#cd_another cons in
    if has_another then None else Some tbl)
  |> List.map reduce
  |> function
  | [] -> failwith "empty tbl"
  | hd :: tl ->
    List.fold_left merge_tbl hd tl
    |> fun result ->
    IdentMap.find_opt parp#ident result
    |> (function
    | Some x ->
      (match x with
       | Var _ -> assert false
       | Constr (desc, values) ->
         assert (Types.may_equal_constr desc varp#desc);
         assert (List.length values = desc.cstr_arity);
         List.fold_left
           (fun acc value ->
             match value with
             | Var x -> IdentMap.add x Delete acc
             | Constr _ -> acc)
           IdentMap.empty
           values)
    | None -> failwith "NOne in prrocess")
;;
