(** Implements client-side aspects of the Initialisation protocol *)

(** Information given to a player (or a spectator) once initialisation is
 * complete, if he has been accepted by the server.
 *)
type data =
  { ident : string
  (** Unique identifier attributed by the server. *)
  ; map : Data.map
  (** Game map *)
  ; params : Protocol.Initialisation.params
  (** Game parameters *)
  ; players : (string, (string * char)) Hashtbl.t
  (** Table mapping identifiers of players to a couple [(pseudo, map_id)] where
   * [pseudo] is the pseudo given by the player and [map_id] is the identifier
   * of the player on the map.
   *)
  ; spectators : (string, string) Hashtbl.t
  (** Table mapping identifiers of spectators to their pseudo. *)
  ; start : Unix.tm * int
  (** Game start date *)
  }

(** Result of a tentative to authenticate on the server. *)
type result =
  | Rejected of string
  (** The server rejected the authentication. *)
  | Ok of data
  (** The server accepted the authentication and returns the subsequence
   * information. *)
  | Closed
  (** Initialisation couldn't be completed, either because the server
   * unexpectedly closed the connection or the user aborted it on the device. *)

(** [hello device ~pseudo ?versions addr] tries to register as a player on a
 * game server listening on address [addr], interacting with the user on device
 * [device].
 * @param pseudo Human-readable pseudonym to identify the player.
 * @param versions Supported versions of the game protocol (default [[1]]).
 *)
val hello :
  Display.Init.t -> pseudo:string -> ?versions:int list -> Network.addr ->
    result Lwt.t

(** [spectator device ~pseudo addr] tries to register as a spectator on the game
 * server listening on address [addr], interacting with the user on device
 * [device].
 * @param pseudo Human-readable pseudonym to identify the spectator.
 *)
val spectator :
  Display.Init.t -> pseudo:string -> Network.addr -> result Lwt.t
