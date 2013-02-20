type data =
  { ident : string
  ; map : Data.map
  ; params : Protocol.Initialisation.params
  ; players : (string, (string * char)) Hashtbl.t
  ; spectators : (string, string) Hashtbl.t
  ; start : Unix.tm * int
  }

type result =
  | Rejected of string
  | Ok of data
  | Closed

val hello : pseudo:string -> ?versions:int list -> Network.addr -> result Lwt.t

val spectator :
  pseudo:string -> Network.addr -> result Lwt.t
