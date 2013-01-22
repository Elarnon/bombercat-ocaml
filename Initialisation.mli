type game =
  { g_map : Data.map
  ; g_params : Protocol.Initialisation.params
  ; g_players : (string, string * char) Hashtbl.t
  ; g_start : Unix.tm * int
  ; g_name : string
  }

module Server : sig

  (* type server *)

  val create : Network.addr -> Meta.Client.Connection.t -> game Lwt.t

  (* val shutdown : server -> unit *)

end

module Client : sig
  type t

  type event =
    [ `Start of Unix.tm * int
    | `Join of string * string * char
    | `Quit of string
    | `Closed ]

  val connect : Network.addr -> t Lwt.t

  val hello : t -> pseudo:string -> versions:int list ->
    [ `Rejected of string
    | `Ok of string * Data.map * Protocol.Initialisation.params ] option Lwt.t

  val poll : t -> event option Lwt.t

end
