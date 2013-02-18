open Lwt
open Protocol.Meta
open Misc
module S = Lwt_stream

module Server = struct

  module Connection = Network.TCP.Make(Protocol.Meta.Server)

  (* Internal structure to represent a table of games *)
  module Games = struct

    type ('a, 'b) t =
      { tbl : ('a, 'b) Hashtbl.t
      ; mutable next_id : int
      ; ids : (Connection.client, 'a list) Hashtbl.t
      ; mutable running : bool
      }

    let create n =
      { tbl = Hashtbl.create n
      ; next_id = 1
      ; ids = Hashtbl.create n
      ; running = true
      }

    let add servers client id game =
      Hashtbl.add servers.tbl id game;
      let client_ids =
	try Hashtbl.find servers.ids client with Not_found -> []
      in Hashtbl.replace servers.ids client (id :: client_ids);
      return ()

    let find servers id = wrap2 Hashtbl.find servers.tbl id

    let games servers =
      wrap3 Hashtbl.fold (fun _ v l -> v :: l) servers.tbl []

    let update servers id game =
      wrap3 Hashtbl.replace servers.tbl id game

    let remove servers id =
      wrap2 Hashtbl.remove servers.tbl id

    let next_id servers =
      servers.next_id <- servers.next_id + 1;
      return servers.next_id

    let take_ids servers client =
      try_lwt
	let v = Hashtbl.find servers.ids client
	in Hashtbl.remove servers.ids client;
	return v
      with Not_found -> return []

    let remove_from servers client =
      lwt ids = take_ids servers client in
      Lwt_list.iter_p (remove servers) ids
  end

  type server =
    { server : Lwt_io.server
    ; games : (int, Protocol.Meta.game) Games.t }

  let treat_client servers client stream =
    S.flatten (S.from begin fun () ->
      S.get stream >>= function
        | None -> return_none
        | _ when not servers.Games.running -> return_none
        | Some (ADD (addr, name, nb_players)) ->
            lwt id = Games.next_id servers in
            let game = 
              { game_id = id
              ; game_addr = addr
              ; game_name = name
              ; game_nb_players = 0
              ; game_max_players = nb_players
              } in
            Games.add servers client id game >>
            return (Some [ADDED id])
        | Some (UPDATE (id, game_nb_players)) -> begin
            try_lwt
              lwt game = Games.find servers id in
              Games.update servers id { game with game_nb_players } >>
              return (Some [])
            with Not_found -> return (Some [])
        end
        | Some (DELETE id) -> Games.remove servers id >> return (Some [])
        | Some LIST ->
            Games.games servers >|= fun g -> Some [GAMES g]
    end)

  let create addr =
    let servers = Games.create 17 in
    { server = Connection.establish_server
        ~close:(fun cli -> Games.remove_from servers cli)
        addr
        (treat_client servers)
    ; games = servers }

  let shutdown { server; games } =
    games.Games.running <- false;
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
          Lwt_log.error
            ("Meta protocol error: `GAMES' message received where a `ADDED' "
            ^"message was expected.") >>
          return_none
      | None ->
          Lwt_log.info
            ("Meta server closed the connection.") >>
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
          Lwt_log.error
            ("Meta protocol error: `ADDED' message received where a `GAMES'"
            ^" message was expected.") >>
          return_none
      | None ->
          Lwt_log.info
            ("Meta server closed the connection.") >>
          return_none
end
