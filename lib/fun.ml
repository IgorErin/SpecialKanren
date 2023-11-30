type 'a t =
  { name : Ident.t
  ; params : Ident.t list
  ; body : 'a
  }

type 'a spec =
  { func : 'a t
  ; spec : Spec.info
  }

let equal { func = fst_func; spec = fst_spec } { func = snd_func; spec = snd_spec } =
  Ident.same fst_func.name snd_func.name && Spec.equal fst_spec snd_spec
;;

let canren_to_dnf ({ func; _ } as f) =
  let body = Dnf.of_canren func.body in
  { f with func = { func with body } }
;;

let map ~f fspec = { fspec with func = { fspec.func with body = f fspec.func.body } }
let chname ~f ~name = { f with func = { f.func with name } }
