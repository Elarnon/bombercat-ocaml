open Lwt
open Protocol.Meta
open Misc
module S = Lwt_stream

module Connection = Network.TCP.Make(Protocol.Meta.Server)

type server =
  { server : Lwt_io.server
  ; running : bool ref }

let treat_client games ids running next_id client stream =
  S.flatten (S.from begin fun () ->
    S.get stream >>= function
      (* Input stream was closed, close the connection *)
      | None ->
          Lwt_log.debug "[Meta server] Connection lost." >>
          return_none
      (* Server was asked to stop *)
      | _ when not !running ->
          Lwt_log.debug "[Meta server] Server stopped, closing connection." >>
          return_none
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
       * registered by the client asking to update it, do nothing otherwise. *)
      | Some (UPDATE (id, game_nb_players)) ->
          if List.mem id (Hashtbl.find_all ids client) then
            begin try
              let game = Hashtbl.find games id in
              Hashtbl.replace games id { game with game_nb_players }
            with Not_found ->
              Lwt.ignore_result begin Lwt_log.debug
                "[Meta server] Trying to UPDATE inexistent server."
              end
            end
          else Lwt.ignore_result begin Lwt_log.debug
            "[Meta server] Client trying to UPDATE a server it doesn't own."
          end;
          return @$ Some []
      (* Treat DELETE message: remove the game from the table if it was
       * registered by the client asking for its deletion. *)
      | Some (DELETE id) ->
          if List.mem id (Hashtbl.find_all ids client) then
            Hashtbl.remove games id
          else Lwt.ignore_result begin Lwt_log.debug
            "[Meta server] Client trying to DELETE a server it doesn't own.";
          end;
          return (Some [])
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
          List.iter (Hashtbl.remove games) (Hashtbl.find_all ids cli);
          Hashtbl.remove ids cli)
        addr
        (treat_client games ids running (ref 1))
  ; running
  }

let shutdown { server; running } =
  running := false;
  Lwt_io.shutdown_server server
