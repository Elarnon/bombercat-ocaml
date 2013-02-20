module Connection : Network.TCP.S

(** Registers a game server on a meta server.
 *  @param connection The connection to the meta server
 *  @param addr An accessible address where the game server will be available
 *  @param name A name to identify the game server
 *  @param nb_players The number of slots available on the map
 *  @return [Some game] if the meta server accepts the game and returns an ID
 *  @return [None] otherwise (bad answer from the meta server, connection
 *          closed, etc.)
 *)
val add :
  Connection.t -> addr:Network.addr -> name:string -> nb_players:int
  -> Protocol.Meta.game option Lwt.t

(** Updates the number of players in a game on a meta server.
 *  @param connection The connection to the meta server
 *  @param id The ID of the game to update
 *  @param nb_players The current number of players in the game
 *)
val update : Connection.t -> id:int -> nb_players:int -> unit

(** Deletes a game from a meta server.
 *  @param connection The connection to the meta server
 *  @param id The ID of the game to delete
 *)
val delete : Connection.t -> id:int -> unit

(** Lists the games on a meta server
 *  @param connection The connection to the meta server
 *  @return [Some lst] where [lst] is the list of all games on the server if
 *          the meta server returns it
 *  @return [None] otherwise (bad answer from the meta server, connection
 *          closed, etc.)
 *)
val list_games : Connection.t -> Protocol.Meta.game list option Lwt.t

(** Runs a meta client on a display and returns the selected game.
 *  @param display The display to use
 *  @param addr The address of the meta server to contact
 *  @return [Some game] if a game was selected
 *  @return [None] otherwise (connection lost, client exited, etc.)
 *)
val run : Display.Meta.t -> Network.addr -> Protocol.Meta.game option Lwt.t
