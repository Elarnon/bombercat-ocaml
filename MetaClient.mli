(** Implements client-side aspects of the Meta protocol *)

module Connection : Network.TCP.S

(** [add co ~addr ~name ~nb_players] registers a game server on the meta server
 * connected with [co].
 *  @param addr An accessible address where the game server will be available
 *  @param name A name to identify the game server
 *  @param nb_players The number of slots available on the map
 *  @return [Some game] if the meta server accepts the game and returns an ID ;
 *          [None] otherwise.
 *  @raise Unix.unix_error on network errors
 *)
val add :
  Connection.t -> addr:Network.addr -> name:string -> nb_players:int
  -> Protocol.Meta.game option Lwt.t

(** [update co ~id ~nb_players] sets the number of players to [nb_players] in
 * the game with identifier [id] on the meta server connected with [co].
 *  @param id The identifier of the game to update
 *  @param nb_players The current number of players in the game
 *  @raise Unix.unix_error on network errors
 *)
val update : Connection.t -> id:int -> nb_players:int -> unit

(** [delete co ~id] deletes the game with identifier [id] from the meta server
 * connected with [co].
 *  @param id The ID of the game to delete
 *  @raise Unix.unix_error on network errors
 *)
val delete : Connection.t -> id:int -> unit

(** [list_games co] lists the games present on the meta server connected with
 * [co].
 *  @return [Some lst] where [lst] is the list of all games on the server if it
 *          is given by the server, [None] otherwise.
 *  @raise Unix.unix_error on network errors
 *)
val list_games : Connection.t -> Protocol.Meta.game list option Lwt.t

(** [run display addr] runs a meta client on display [display], connecting to
 * the meta server at address [addr], and returns the game that was selected.
 * [display] is closed on any network error.
 * @return [Some game] if a game was selected, [None] otherwise.
 * @raise Unix.unix_error on network error
 *)
val run : Display.Meta.t -> Network.addr -> Protocol.Meta.game option Lwt.t
