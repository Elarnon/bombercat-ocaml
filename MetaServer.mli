(** A server for the Meta protocol. *)
type server

(** Creates a new server for the Meta protocol.
 *  @param addr The address the server listens to
 *)
val create : Network.addr -> server

(** Stops a given server for the Meta protocol.
 *  @param server The server to stop
 *)
val shutdown : server -> unit
