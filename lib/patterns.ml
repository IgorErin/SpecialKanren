type ('a, 'b, 'c) pat = Pat of ('a -> 'b -> 'c)
type reason = Fail of string

exception Expected of reason

let fail str = raise @@ Expected (Fail str)

let parse (Pat f : ('a, 'b, 'c) pat) (x : 'a) (k : 'b) (e : 'c) =
  try f x k with
  | Expected (Fail _) -> e
;;

let parse_bool p (x : 'a) = parse p x true false

open Typedtree

module Gen = struct
  let var = Pat (fun x k -> k x)
  let __ = var

  let alt (Pat first) (Pat second) =
    Pat
      (fun x k ->
        try first x k with
        | Expected (Fail _) ->
          (try second x k with
           | Expected (Fail _) -> fail "alt"))
  ;;

  let ( <|> ) = alt
  let drop : 'a 'b. ('a, 'b, 'b) pat = Pat (fun _ k -> k)

  let lscons (Pat hd') (Pat tl') =
    Pat
      (fun x k ->
        match x with
        | hd :: tl -> k |> hd' hd |> tl' tl
        | _ -> fail "list cons")
  ;;

  let ( <::> ) = lscons

  let lsnil =
    Pat
      (fun x k ->
        match x with
        | [] -> k
        | _ -> fail "list nil")
  ;;

  let list ps =
    let rec helper xs ps k =
      match xs, ps with
      | [], [] -> k
      | hdx :: tlx, Pat hdp :: tlp ->
        let _ = hdp hdx k in
        helper tlx tlp k
      | _ -> fail "list"
    in
    Pat (fun x k -> helper x ps k)
  ;;

  let list_closer (Pat p) =
    let rec helper xs k =
      match xs with
      | hd :: tl ->
        p hd k;
        helper tl k
      | [] -> ()
    in
    Pat (fun x k -> helper x k)
  ;;

  let pair (Pat fstp) (Pat sndp) = Pat (fun (fst, snd) k -> k |> fstp fst |> sndp snd)

  let get f =
    Pat
      (fun x k ->
        f x;
        k)
  ;;

  let some (Pat xp) =
    Pat
      (fun x k ->
        match x with
        | Some x -> xp x k
        | None -> fail "Some")
  ;;

  let none =
    Pat
      (fun x k ->
        match x with
        | None -> k
        | Some _ -> fail "None")
  ;;

  let str s = Pat (fun x k -> if String.equal x s then k else fail "String")
  let map f (Pat p) = Pat (fun x k -> p x (fun a -> k (f a)))
  let map2 f (Pat p) = Pat (fun x k -> p x (fun a b -> k (f a b)))
end

let expression
  (Pat desc')
  (Pat loc')
  (Pat extra')
  (Pat type')
  (Pat env')
  (Pat attributes')
  =
  Pat
    (fun x k ->
      let { exp_desc : expression_desc
          ; exp_loc : Location.t
          ; exp_extra : (exp_extra * Location.t * attributes) list
          ; exp_type : Types.type_expr
          ; exp_env : Env.t
          ; exp_attributes : attributes
          }
        =
        x
      in
      k
      |> desc' exp_desc
      |> loc' exp_loc
      |> extra' exp_extra
      |> type' exp_type
      |> env' exp_env
      |> attributes' exp_attributes)
;;

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
        | _ -> fail "Structure_item_desc.Tstr_value")
  ;;

  let tstr_open (Pat dec) =
    Pat
      (fun x k ->
        match x with
        | Tstr_open x -> dec x k
        | _ -> fail "Structure_item_desc.Tstr_open")
  ;;
end

let value_binding (Pat p) (Pat e) (Pat a) (Pat l) =
  Pat
    (fun x k ->
      let { vb_pat : pattern
          ; vb_expr : expression
          ; vb_attributes : attributes
          ; vb_loc : Location.t
          }
        =
        x
      in
      k |> p vb_pat |> e vb_expr |> a vb_attributes |> l vb_loc)
;;

let pattern_data (Pat desc) (Pat loc) (Pat extra) (Pat type') (Pat env) (Pat attributes) =
  Pat
    (fun x k ->
      let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = x in
      k
      |> desc pat_desc
      |> loc pat_loc
      |> extra pat_extra
      |> type' pat_type
      |> env pat_env
      |> attributes pat_attributes)
;;

module Pattern_desc = struct
  let tpat_var (Pat ident) (Pat loc) =
    Pat
      (fun x k ->
        match x with
        | Tpat_var (ident_value, loc_value) -> k |> ident ident_value |> loc loc_value
        | _ -> fail "Pattern_desc.Tpat_var")
  ;;
end

module Partial = struct
  let partial =
    Pat
      (fun x k ->
        match x with
        | Partial -> k
        | _ -> fail "Partial.partial")
  ;;

  let total =
    Pat
      (fun x k ->
        match x with
        | Total -> k
        | _ -> fail "Partial.total")
  ;;
end

module Expression_desc = struct
  let texp_ident (Pat path') (Pat loc') (Pat t_vd') =
    Pat
      (fun x k ->
        match x with
        | Texp_ident (path, loc, t_vd) -> k |> path' path |> loc' loc |> t_vd' t_vd
        | _ -> fail "Expression_desc.Texp_ident")
  ;;

  let texp_function (Pat arg') (Pat param') (Pat cases') (Pat partial') =
    Pat
      (fun x k ->
        match x with
        | Texp_function { arg_label; param; cases; partial } ->
          k |> arg' arg_label |> param' param |> cases' cases |> partial' partial
        | _ -> fail "Expression_desc.Texp_function")
  ;;

  let texp_apply (Pat e') (Pat xs') =
    Pat
      (fun x k ->
        match x with
        | Texp_apply (e, xs) -> k |> e' e |> xs' xs
        | _ -> fail "Expression_desc.Texp_apply")
  ;;

  let texp_construct (Pat loc') (Pat cons_desc') (Pat expl') =
    (* TODO (Types.constructor_description in cons_desc)*)
    Pat
      (fun x k ->
        match x with
        | Texp_construct (loc, cons_des, expl) ->
          k |> loc' loc |> cons_desc' cons_des |> expl' expl
        | _ -> fail "Expression_desc.Texp_construct")
  ;;
end

let case (Pat first) (Pat second) (Pat third) =
  Pat
    (fun x k ->
      let { c_lhs : _ general_pattern; c_guard : expression option; c_rhs : expression } =
        x
      in
      k |> first c_lhs |> second c_guard |> third c_rhs)
;;

module Arg_lable = struct
  (* Asttypes module *)
  let noLable =
    Pat
      (fun x k ->
        match x with
        | Asttypes.Nolabel -> k
        | _ -> fail "Arg_lable.NoLable")
  ;;

  let labelled (Pat label') =
    Pat
      (fun x k ->
        match x with
        | Asttypes.Labelled label -> k |> label' label
        | _ -> fail "Arg_lable.Labelled")
  ;;

  let optional (Pat label') =
    Pat
      (fun x k ->
        match x with
        | Asttypes.Optional label -> k |> label' label
        | _ -> fail "Arg_lable.Optional")
  ;;
end

module PathPat = struct
  let extract path =
    let rec loop acc = function
      | Path.Pident x -> Ident.name x :: acc
      | Path.Pdot (next, x) ->
        let acc = x :: acc in
        loop acc next
      | Path.Papply _ -> failwith "Try to extract path from apply"
    in
    loop [] path
  ;;

  let pident ident =
    Pat
      (fun x k ->
        match x with
        | Path.Pident x when Ident.same x ident -> k
        | _ -> fail "Path.Pident")
  ;;

  let pdot (Pat path') (Pat str') =
    Pat
      (fun x k ->
        match x with
        | Path.Pdot (path, str) -> k |> path' path |> str' str
        | _ -> fail "Path.Pdot")
  ;;

  let papply (Pat left') (Pat right') =
    Pat
      (fun x k ->
        match x with
        | Path.Papply (left, right) -> k |> left' left |> right' right
        | _ -> fail "Path.Papply")
  ;;

  let match' (Pat fxs) =
    Pat
      (fun x k ->
        let path = extract x in
        fxs path k)
  ;;
end

let structuere_item (Pat desc) (Pat loc) (Pat env) =
  Pat
    (fun x k ->
      let { str_desc : structure_item_desc; str_loc : Location.t; str_env : Env.t } = x in
      k |> desc str_desc |> loc str_loc |> env str_env)
;;

let open_infos
  (Pat expr)
  (Pat bound_items)
  (Pat override)
  (Pat env)
  (Pat loc)
  (Pat attributes)
  =
  Pat
    (fun x k ->
      let { open_expr
          ; open_bound_items : Types.signature
          ; open_override : Asttypes.override_flag
          ; open_env : Env.t
          ; open_loc : Location.t
          ; open_attributes : attribute list
          }
        =
        x
      in
      k
      |> expr open_expr
      |> bound_items open_bound_items
      |> override open_override
      |> env open_env
      |> loc open_loc
      |> attributes open_attributes)
;;

let module_expr (Pat desc) (Pat loc) (Pat type') (Pat env) (Pat attributes) =
  Pat
    (fun x k ->
      let { mod_desc : module_expr_desc
          ; mod_loc : Location.t
          ; mod_type : Types.module_type
          ; mod_env : Env.t
          ; mod_attributes : attributes
          }
        =
        x
      in
      k
      |> desc mod_desc
      |> loc mod_loc
      |> type' mod_type
      |> env mod_env
      |> attributes mod_attributes)
;;

module Module_expr_desc = struct
  let tmod_ident (Pat path') (Pat ident') =
    Pat
      (fun x k ->
        match x with
        | Tmod_ident (path, ident) -> k |> path' path |> ident' ident
        | _ -> fail "Module_expr_desc.Tmod_ident")
  ;;
end

let structure (Pat items) (Pat type') (Pat env) =
  Pat
    (fun x k ->
      let { str_items : structure_item list
          ; str_type : Types.signature
          ; str_final_env : Env.t
          }
        =
        x
      in
      k |> items str_items |> type' str_type |> env str_final_env)
;;

let structure_item (Pat decs) (Pat loc) (Pat env) =
  Pat
    (fun x k ->
      let { str_desc : structure_item_desc; str_loc : Location.t; str_env : Env.t } = x in
      k |> decs str_desc |> loc str_loc |> env str_env)
;;

module Rec_flag = struct
  open Asttypes

  let nonrecursive =
    Pat
      (fun x k ->
        match x with
        | Nonrecursive -> k
        | _ -> fail "Nonrecursive")
  ;;

  let recursive =
    Pat
      (fun x k ->
        match x with
        | Recursive -> k
        | _ -> fail "Recursive")
  ;;
end

module TypesPats = struct
  open Types

  (* Tconstr of Path.t * type_expr list * abbrev_memo ref*)
  module Type_desc = struct
    let tconstr (Pat path') (Pat exp_list') (Pat abbrev') =
      Pat
        (fun x k ->
          match x with
          | Tconstr (path, typ_exp_list, abbrev_memo) ->
            k |> path' path |> exp_list' typ_exp_list |> abbrev' abbrev_memo
          | _ -> fail "Type_desc.Tconstr")
    ;;
  end

  let constructor_description (Pat name) =
    (* TODO more fields *)
    let open Asttypes in
    Pat
      (fun x k ->
        let { cstr_name : string (* Constructor name *); _ } = x in
        name cstr_name k)
  ;;
end

let ident name =
  Pat (fun x k -> if String.equal name @@ Ident.name x then k else fail "Ident")
;;
