open Canren

type ('a, 'b) item =
  | DUnify of ('a * 'b)
  | DDisunify of ('a * 'b)
  | DCall of (Path.t * Value.t list)
  | DFresh of Ident.t list

type ('a, 'b) cnj = ('a, 'b) item list
type ('a, 'b) dnf = ('a, 'b) cnj list

let of_canren canren =
  let wrap x = x |> Core.List.return |> Core.List.return in
  let rec loop = function
    | Conj (left, right) ->
      Core.List.cartesian_product (loop left) (loop right)
      |> List.map (fun (fst, snd) -> fst @ snd)
    | Disj (left, right) -> loop left @ loop right
    | Unify (left, right) -> DUnify (left, right) |> wrap
    | Disunify (left, right) -> DDisunify (left, right) |> wrap
    | Call (name, values) -> DCall (name, values) |> wrap
    | Fresh (freshs, next) -> List.map (fun item -> [ DFresh freshs ] @ item) @@ loop next
  in
  loop canren
;;

let pp f pfst psnd =
  List.iter (fun l ->
    Format.printf "\n new disj \n";
    List.iter
      (fun x ->
        Format.printf " && ";
        match x with
        | DUnify (fst, snd) ->
          pfst f fst;
          Format.fprintf f "===";
          psnd f snd
        | DDisunify (fst, snd) ->
          Format.fprintf f "(";
          pfst f fst;
          Format.fprintf f "=/=";
          psnd f snd
        | DCall (ident, values) ->
          Format.fprintf f "%s (" @@ Path.name ident;
          List.iter (fun v -> Format.fprintf f "%s" @@ Value.to_string v) values;
          Format.fprintf f ")"
        | DFresh freshs ->
          Format.fprintf f "Fresh (";
          List.iter (fun i -> Format.fprintf f "%s " @@ Ident.name i) freshs;
          Format.fprintf f ")")
      l)
;;
