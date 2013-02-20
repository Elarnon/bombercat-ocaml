(** Implements server-side aspects of the Initialisation protocol *)

(** Type representing a game *)
type game =
  { g_map : Data.map
  (** Game map *)
  ; g_params : Protocol.Initialisation.params
  (** Game parameters *)
  ; g_players : (string, string * char) Hashtbl.t
  (** Mapping from unique player identifier to their pseudonym and identifier on
   * the game map. *)
  ; g_start : Unix.tm * int
  (** Game start date *)
  ; g_name : string
  (** Human-readable name of the game. *)
  }

(** [create addr meta] creates a new initialisation server listening on address
 * [addr] and registering on the meta server at address [meta].
 * @return [Some game] once the initalisation is complete, [None] if an error
 * occured (for instance, if the Meta server was closed or not reachable in the
 * first place).
 *)
(* TODO: change MetaClient.Connection.t to Network.addr *)
(* TODO: take name and parameters as arguments *)
val create : Network.addr -> MetaClient.Connection.t -> game option Lwt.t
