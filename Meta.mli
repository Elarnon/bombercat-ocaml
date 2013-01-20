module Server : sig
  (** Type representing a Meta server *)
  type server

  (** Creates a new Meta server listening on the given address *)
  val create : Network.addr -> server

  (** Closes a Meta server *)
  val shutdown : server -> unit

end

module Client : sig
  module Connection : Network.TCP.S

  (** Send an ADD message to the Meta server connected and returns the
   * answer of the server *)
  val add :
    Connection.t -> addr:Network.addr -> name:string -> nb_players:int
    -> Protocol.Meta.game option Lwt.t

  (** Asynchronously send an UPDATE message to the Meta server *)
  val update : Connection.t -> id:int -> nb_players:int -> unit

  (** Asynchronously send a DELETE message to the Meta server *)
  val delete : Connection.t -> id:int -> unit

  (** List games from the Meta server *)
  val list_games : Connection.t -> Protocol.Meta.game list option Lwt.t
end 
