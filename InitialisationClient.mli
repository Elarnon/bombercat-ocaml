type t

type event =
  [ `Start of Unix.tm * int
  | `Join of string * string * char
  | `Spectator of string * string
  | `Quit of string
  | `Closed ]

val connect : Network.addr -> t Lwt.t

val hello : t -> pseudo:string -> versions:int list ->
  [ `Rejected of string
  | `Ok of string * Data.map * Protocol.Initialisation.params ] option Lwt.t

val spectator : t -> pseudo:string ->
  [ `Rejected of string
  | `Ok of string * Data.map * Protocol.Initialisation.params ] option Lwt.t

val poll : t -> event option Lwt.t
