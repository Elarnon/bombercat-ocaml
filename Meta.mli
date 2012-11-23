val server : Network.addr -> Lwt_io.server

module Client : sig
  module TCP : Network.TCP.S

  val add :
    TCP.t -> addr:Network.addr -> name:string -> nb_players:int
    -> Protocol.Meta.game option Lwt.t

  val update : TCP.t -> id:int -> nb_players:int -> unit Lwt.t

  val delete : TCP.t -> id:int -> unit Lwt.t

  val list_games : TCP.t -> Protocol.Meta.game list option Lwt.t
end 
