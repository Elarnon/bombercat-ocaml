(** Implements server-side aspects of the Meta protocol. *)

(** Represents a server for the Meta protocol. *)
type server

(** [create addr] creates a new server for the Meta protocol, listening on
 * address [addr].
 *)
val create : Network.addr -> server

(** [shutdown server] closes the Meta server [server], that is, [server] stops
 * accepting new connections and existing connections are closed.
 *)
val shutdown : server -> unit
