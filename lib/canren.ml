open Ocanren_patterns
open Value

type error = Unexpected_ast_structure of string

exception Error of error

let error e = raise @@ Error e

module Utils = struct
  let get_path exp =
    let open Typedtree in
    exp.exp_desc
    |> function
    | Texp_ident (path, _, _) -> path
    | _ -> error @@ Unexpected_ast_structure "Path expected"
  ;;

  let get_value vars exp =
    let open Typedtree in
    let is_var ident = List.exists (Ident.same ident) vars in
    let rec loop exp =
      match exp.exp_desc with
      | Texp_construct (_, cons_desc, args) ->
        args |> List.map loop |> fun args -> Value.Constr (cons_desc, args)
      | Texp_ident (Path.Pident ident, _, _) when is_var ident -> Var ident
      | Texp_apply (hd, args) when Ocanren_patterns.is_inj hd ->
        let arg = Assert.un_arg args in
        loop arg
      | Texp_ident _ -> error @@ Unexpected_ast_structure "Seems like dot ident in values"
      | _ -> error @@ Unexpected_ast_structure "Value expected"
    in
    loop exp
  ;;

  let get_params exp =
    let open Typedtree in
    let rec loop acc exp =
      match exp.exp_desc with
      | Texp_function { param; cases = [ { c_rhs; _ } ]; _ } -> loop (param :: acc) c_rhs
      | Texp_function _ ->
        error @@ Unexpected_ast_structure "Unexpected nontrivial branching"
      | _ -> exp, acc
    in
    loop [] exp
  ;;
end

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

let of_tast exp =
  let open Typedtree in
  let rec loop (vars : Ident.t list) exp =
    match exp.exp_desc with
    | Texp_function _ -> error @@ Unexpected_ast_structure "Assumed no functions."
    | Texp_apply (hd_exp, args) when is_conde hd_exp ->
      let e = Assert.un_arg args in
      loop vars e
    (* (::) list cons. assume disj *)
    | Texp_construct (_, _, args) when is_list_cons exp ->
      let fst, snd = Assert.bin args in
      let fst = loop vars fst in
      let snd = loop vars snd in
      Core.Option.merge fst snd ~f:(fun fst snd -> Disj (fst, snd))
    (* (|||) disj *)
    | Texp_apply (hd_exp, args) when is_disj hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fexp = loop vars fexp in
      let sexp = loop vars sexp in
      Core.Option.merge fexp sexp ~f:(fun fexp sexp -> Disj (fexp, sexp))
      (* (&&&) conj *)
    | Texp_apply (hd_exp, args) when is_conj hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fexp = loop vars fexp in
      let sexp = loop vars sexp in
      Core.Option.merge fexp sexp ~f:(fun fexp sexp -> Conj (fexp, sexp))
    (* === *)
    | Texp_apply (hd_exp, args) when is_unify hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fst = Utils.get_value vars fexp in
      let snd = Utils.get_value vars sexp in
      Option.some (Unify (fst, snd))
    (* =/= *)
    | Texp_apply (hd_exp, args) when is_nunify hd_exp ->
      let fexp, sexp = Assert.bin_args args in
      let fst = Utils.get_value vars fexp in
      let snd = Utils.get_value vars sexp in
      Option.some (Disunify (fst, snd))
      (* fresh *)
    | Texp_apply (hd, args) when is_fresh hd ->
      let e = Assert.un_arg args in
      let exp, new_fresh = Utils.get_params e in
      let vars = vars @ new_fresh in
      Option.map (fun x -> Fresh (List.rev new_fresh, x)) @@ loop vars exp
    | Texp_apply (hd_exp, args) ->
      let args = Assert.args args in
      let args = List.map (Utils.get_value vars) args in
      let ident = Utils.get_path hd_exp in
      Call (ident, args) |> Option.some
    (* paramter -> variant *)
    | _ -> None (* hack to hold :: in conde. rework *)
  in
  let body, global = Utils.get_params exp in
  let global = List.rev global in
  global, loop global body
;;

let get_declared_fresh_vars e =
  let rec loop acc = function
    | Fresh (vars, next) -> loop (vars @ acc) next
    | Call (_, _) -> acc
    | Unify _ | Disunify _ -> acc
    | Conj (left, right) | Disj (left, right) -> loop acc left @ loop acc right
  in
  loop [] e
;;
