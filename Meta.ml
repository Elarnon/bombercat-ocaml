open Protocol.Meta
open Network

module Integer = struct
  type t = int
  let compare = compare
end

module Imap = Map.Make(Integer)

module Server = struct

  (* TODO: check provenance *)
  let treat_client client (servers, next_id) = function
    | ADD (addr, name, nb_players) ->
        (* TODO: limit the number of games *)
        let id = next_id in
        let game =
          { game_id = id
          ; game_addr = addr
          ; game_name = name
          ; game_nb_players = nb_players
          } in
        Protocol.Meta.Server.write client (ADDED next_id);
        (* TODO: broadcast GAMES ? *)
        (Imap.add id game servers, next_id + 1)
    | UPDATE (id, nb_players) -> begin
        try
          let game = Imap.find id servers in
          (Imap.add id { game with game_nb_players = nb_players } servers,
          next_id)
        with Not_found -> (servers, next_id)
    end
    | DELETE id ->
        (Imap.remove id servers, next_id)
    | LIST ->
        let games = Imap.fold (fun _ g l -> g :: l) servers []
        in Protocol.Meta.Server.write client (GAMES games);
        (servers, next_id)

        (*
  let treat_all clients = 
    List.fold_left
      (servers, next_id)
      clients

  let rec step serv clients =
    check_ops ~accept:[serv] ~read:clients ~write:clients ();
    match accept_if_possible serv with
    | Some client -> (* Incoming connection *)
        step serv (client :: clients)
    | None ->
        let new_clients = treat_all clients in (* Don't forget deconnections *)
        send_all new_clients; (* Flush messages *)
        step serv clients

  let main addr =
    Unix.handle_unix_error (fun () ->
      let chan = bind (mk_socket ()) addr in
      step chan []) ()
*)
end
