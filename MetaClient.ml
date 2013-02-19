open Lwt
open Protocol.Meta
open Misc
module S = Lwt_stream

module Connection = Network.TCP.Make(Protocol.Meta.Client)

let add srv ~addr ~name ~nb_players =
  Connection.send srv (ADD (addr, name, nb_players));
  Connection.recv srv >>= function
    | Some (ADDED game_id) -> return @$ Some
        { game_id; game_addr = addr; game_name = name
        ; game_nb_players = 0 ; game_max_players = nb_players }
    | Some (GAMES _) -> 
        Lwt_log.info
          ("[Meta client] `GAMES' message received in answer to an"
          ^" `ADD' message (expected `ADDED'). Aborting the connection.") >>
        return_none
    | None ->
        Lwt_log.info ("[Meta client] Server shut down the connection.") >>
        return_none

let update srv ~id ~nb_players =
  Connection.send srv (UPDATE (id, nb_players))

let delete srv ~id =
  Connection.send srv (DELETE id)

let list_games srv =
  Connection.send srv LIST;
  Connection.recv srv >>= function
    | Some (GAMES games) ->
        return @$ Some games
    | Some (ADDED _) ->
        Lwt_log.info
          ("[Meta client] `ADDED' messages received in answer to a"
          ^" `LIST' message (expected `GAMES'). Aborting the connection.") >>
        return_none
    | None ->
        Lwt_log.info ("[Meta client] Server shut down the connection.") >>
        return_none
