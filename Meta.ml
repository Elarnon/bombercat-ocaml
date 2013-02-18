open Lwt
open Protocol.Meta
open Misc
module S = Lwt_stream

module Server = struct

  module Connection = Network.TCP.Make(Protocol.Meta.Server)

  type server =
    { server : Lwt_io.server
    ; running : bool ref }

  let treat_client games ids running next_id client stream =
    S.flatten (S.from begin fun () ->
      S.get stream >>= function
        (* Input stream was closed, stop the server *)
        | None -> return_none
        (* Server was asked to stop *)
        | _ when not !running -> return_none
        (* Treat ADD message: generate ID, register the game, and answer that
         * the game was ADDED. *)
        | Some (ADD (addr, name, nb_players)) ->
            let id = !next_id in
            next_id := !next_id + 1;
            let game = 
              { game_id = id
              ; game_addr = addr
              ; game_name = name
              ; game_nb_players = 0
              ; game_max_players = nb_players
              } in
            Hashtbl.add ids client id;
            Hashtbl.replace games id game;
            return (Some [ADDED id])
        (* Treat UPDATE message: updates the game in the table if it was already
         * registered, do nothing otherwise.
         * Note that there is no check that the client UPDATing is the same as
         * the client that ADDed the game in the first place (TODO ?) *)
        | Some (UPDATE (id, game_nb_players)) ->
            begin try_lwt
              let game = Hashtbl.find games id in
              Hashtbl.replace games id { game with game_nb_players };
              return (Some [])
            with Not_found -> return (Some []) end
        (* Treat DELETE message: remove the game from the table if it was
         * registered.
         * Note that there is no check that the client DELETEing is the same as
         * the client that ADDed the game in the first place (TODO ?) *)
        | Some (DELETE id) ->
            Hashtbl.remove games id; return (Some [])
        (* Treat LIST message: lists all available games in a GAMES message. *)
        | Some LIST ->
            return @$ Some [GAMES (Hashtbl.values games)]
    end)

  let create addr =
    let games = Hashtbl.create 17 in
    let ids = Hashtbl.create 17 in
    let running = ref true in
    { server =
        Connection.establish_server
          (* Remove every games registered by a client when it quits *)
          ~close:(fun cli ->
            begin try
              List.iter (Hashtbl.remove games) (Hashtbl.find_all ids cli)
            with Not_found -> () end;
            Hashtbl.remove ids cli)
          addr
          (treat_client games ids running (ref 1))
    ; running
    }

  let shutdown { server; running } =
    running := false;
    Lwt_io.shutdown_server server
end

module Client = struct

  module Connection = Network.TCP.Make(Protocol.Meta.Client)

  let add srv ~addr ~name ~nb_players =
    Connection.send srv (ADD (addr, name, nb_players));
    Connection.recv srv >>= function
      | Some (ADDED game_id) -> return @$ Some
          { game_id; game_addr = addr; game_name = name
          ; game_nb_players = 0 ; game_max_players = nb_players }
      | Some (GAMES games) -> 
          Lwt_log.info
            ("[Meta.Client] `GAMES' message received in answer to an"
            ^" `ADD' message (expected `ADDED'). Aborting the connection.") >>
          return_none
      | None ->
          Lwt_log.info ("[Meta.Client] Server shut down the connection.") >>
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
      | Some (ADDED id) ->
          Lwt_log.info
            ("[Meta.Client] `ADDED' messages received in answer to a"
            ^" `LIST' message (expected `GAMES'). Aborting the connection.") >>
          return_none
      | None ->
          Lwt_log.info ("[Meta.Client] Server shut down the connection.") >>
          return_none
end
