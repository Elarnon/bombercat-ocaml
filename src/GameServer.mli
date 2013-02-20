(** Implements server-side aspects of the Game protocol *)

(** [run addr game] starts a game server listening on address [addr] with game
 * informations from [game].
 *)
val run : Network.addr -> InitialisationServer.game -> unit Lwt.t
