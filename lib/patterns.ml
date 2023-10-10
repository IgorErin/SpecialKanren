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
end

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

(*
   and value_binding =
     {
       vb_pat: pattern;
       vb_expr: expression;
       vb_attributes: attributes;
       vb_loc: Location.t; (TODO())
     }
*)
let value_binding (Pat p) (Pat e) (Pat a) =
  Pat
    (fun x k ->
      let { vb_pat = pat; vb_expr = expr; vb_attributes = atr; vb_loc = _ } =
        x
      in
      k |> p pat |> e expr |> a atr)

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

module Expression_desc = struct
  let texp_function (Pat arg') (Pat param') (Pat cases') (Pat partial') =
    Pat
      (fun x k ->
        match x with
        | Texp_function { arg_label; param; cases; partial } ->
            k |> arg' arg_label |> param' param |> cases' cases
            |> partial' partial
        | _ -> fail ())
end
