type region =
  | Canren
  | Outer
  | Global
  | Local
  | Dnf_past
  | Typespat

exception
  Error of
    { region : region
    ; message : string
    }

let error region message = raise @@ Error { region; message }
let canren = error Canren
let global = error Global
let local = error Local
let outer = error Outer
let dnf_past = error Dnf_past
let typespat = error Typespat

let show = function
  | Canren -> "Canren"
  | Outer -> "Outer"
  | Global -> "Global"
  | Local -> "Local"
  | Dnf_past -> "Dnf_past"
  | Typespat -> "Typespat"
;;
