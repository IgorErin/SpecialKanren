type ('a, 'b, 'c) pat = Pat of ('a -> 'b -> 'c)
type reason = Fail

exception Expected of reason

let fail () = raise @@ Expected Fail
let __ = Pat (fun x k -> k x)

let parse (Pat f : ('a, 'b, 'c) pat) (x : 'a) (k : 'b) (on_error : unit -> 'c) =
  try f x k with Expected Fail -> on_error ()

open Typedtree

module Gen = struct
  let alt (Pat first) (Pat second) =
    Pat
      (fun x k ->
        try first x k
        with Expected Fail -> ( try second x k with Expected Fail -> fail ()))

  let ( <|> ) = alt
  let drop = Pat (fun k _ -> k)
  let list = drop
  (* TODO *)
  (* TODO (how to match list ???) *)
end

let expression (Pat desc') (Pat loc') (Pat extra') (Pat type') (Pat env')
    (Pat attributes') =
  Pat
    (fun x k ->
      let {
        exp_desc : expression_desc;
        exp_loc : Location.t;
        exp_extra : (exp_extra * Location.t * attributes) list;
        exp_type : Types.type_expr;
        exp_env : Env.t;
        exp_attributes : attributes;
      } =
        x
      in
      k |> desc' exp_desc |> loc' exp_loc |> extra' exp_extra |> type' exp_type
      |> env' exp_env |> attributes' exp_attributes)

(* type tructure_item_desc =
     Tstr_eval of expression * attributes
   | Tstr_value of rec_flag * value_binding list // Done
   | Tstr_primitive of value_description
   | Tstr_type of rec_flag * type_declaration list
   | Tstr_typext of type_extension
   | Tstr_exception of type_exception
   | Tstr_module of module_binding
   | Tstr_recmodule of module_binding list
   | Tstr_modtype of module_type_declaration
   | Tstr_open of open_declaration
   | Tstr_class of (class_declaration * string list) list
   | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
   | Tstr_include of include_declaration
   | Tstr_attribute of attribute*)
module Structure_item_desc = struct
  (*Tstr_value of rec_flag * value_binding list*)
  let tstr_value (Pat first) (Pat second) =
    Pat
      (fun x k ->
        match x with
        | Tstr_value (rec_flag, value_bindings) ->
            k |> first rec_flag |> second value_bindings
        | _ -> fail ())

  (* TODO (more patterns)*)
end

let value_binding (Pat p) (Pat e) (Pat a) (Pat l) =
  Pat
    (fun x k ->
      let {
        vb_pat : pattern;
        vb_expr : expression;
        vb_attributes : attributes;
        vb_loc : Location.t;
      } =
        x
      in
      k |> p vb_pat |> e vb_expr |> a vb_attributes |> l vb_loc)

(*
   type pattern = value general_pattern
   and 'k general_pattern = 'k pattern_desc pattern_data
   Note: type value = Value_pattern
*)

let pattern_data (Pat desc) (Pat loc) (Pat extra) (Pat type') (Pat env)
    (Pat attributes) =
  Pat
    (fun x k ->
      let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } =
        x
      in
      k |> desc pat_desc |> loc pat_loc |> extra pat_extra |> type' pat_type
      |> env pat_env |> attributes pat_attributes)

module Pattern_desc = struct
  let tpat_var (Pat ident) (Pat loc) =
    Pat
      (fun x k ->
        match x with
        | Tpat_var (ident_value, loc_value) ->
            k |> ident ident_value |> loc loc_value
        | _ -> fail ())
end

module Partial = struct
  let partial = Pat (fun x k -> match x with Partial -> k | _ -> fail ())
  let total = Pat (fun x k -> match x with Total -> k | _ -> fail ())
end

module Expression_desc = struct
  let texp_ident (Pat path') (Pat loc') (Pat t_vd') =
    (* TODO (type patterns too in t_vd) *)
    Pat
      (fun x k ->
        match x with
        | Texp_ident (path, loc, t_vd) ->
            k |> path' path |> loc' loc |> t_vd' t_vd
        | _ -> fail ())

  let texp_function (Pat arg') (Pat param') (Pat cases') (Pat partial') =
    Pat
      (fun x k ->
        match x with
        | Texp_function { arg_label; param; cases; partial } ->
            k |> arg' arg_label |> param' param |> cases' cases
            |> partial' partial
        | _ -> fail ())

  let texp_apply (Pat e') (Pat xs') =
    Pat
      (fun x k ->
        match x with Texp_apply (e, xs) -> k |> e' e |> xs' xs | _ -> fail ())

  let texp_construct (Pat loc') (Pat cons_desc') (Pat expl') =
    (* TODO (Types.constructor_description in cons_desc)*)
    Pat
      (fun x k ->
        match x with
        | Texp_construct (loc, cons_des, expl) ->
            k |> loc' loc |> cons_desc' cons_des |> expl' expl
        | _ -> fail ())
end

let case (Pat first) (Pat second) (Pat third) =
  Pat
    (fun x k ->
      let {
        c_lhs : 'k general_pattern;
        c_guard : expression option;
        c_rhs : expression;
      } =
        x
      in
      k |> first c_lhs |> second c_guard |> third c_rhs)

module Arg_lable = struct
  (* Asttypes module *)
  let noLable =
    Pat (fun x k -> match x with Asttypes.Nolabel -> k | _ -> fail ())

  let labelled (Pat label') =
    Pat
      (fun x k ->
        match x with
        | Asttypes.Labelled label -> k |> label' label
        | _ -> fail ())

  let optional (Pat label') =
    Pat
      (fun x k ->
        match x with
        | Asttypes.Optional label -> k |> label' label
        | _ -> fail ())
end
