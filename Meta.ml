open Lwt
open Protocol.Meta
open Misc
module S = Lwt_stream

module Server = struct

  module TCP = Network.TCP.Make(Protocol.Meta.Server)

  module Games = struct

    type ('a, 'b) t =
      { tbl : ('a, 'b) Hashtbl.t
      ; mutable next_id : int
      ; ids : (TCP.client, 'a list) Hashtbl.t
      }

    let create n =
      { tbl = Hashtbl.create n
      ; next_id = 1
      ; ids = Hashtbl.create 17
      }

    let add servers client id game =
      Hashtbl.add servers.tbl id game;
      let v =
	try Hashtbl.find servers.ids client with Not_found -> []
      in Hashtbl.replace servers.ids client (id :: v);
      return ()

    let find servers id =
      try
	return (Hashtbl.find servers.tbl id)
      with Not_found -> fail Not_found

    let games servers =
      return (Hashtbl.fold (fun _ v l -> v :: l) servers.tbl [])

    let update servers id game =
      return (Hashtbl.replace servers.tbl id game)

    let remove servers id =
      return (Hashtbl.remove servers.tbl id)

    let next_id servers =
      servers.next_id <- servers.next_id + 1;
      return servers.next_id

    let take_ids servers client =
      try
	let v = Hashtbl.find servers.ids client
	in Hashtbl.remove servers.ids client;
	return v
      with Not_found -> return []
  end

  let remove_from servers client =
    Games.take_ids servers client >>= fun ids ->
    Lwt_list.iter_p (fun id -> Games.remove servers id) ids

  let treat_client servers client stream =
    S.flatten (S.from begin fun () ->
      S.get stream >>= function
        | None -> return None
        | Some (ADD (addr, name, nb_players)) ->
            Games.next_id servers >>= fun id ->
            let game = 
              { game_id = id
              ; game_addr = addr
              ; game_name = name
              ; game_nb_players = nb_players
              } in
            Games.add servers client id game >>= fun () ->
            return (Some [ADDED id])
        | Some (UPDATE (id, nb_players)) -> begin
            catch (fun () ->
              Games.find servers id >>= fun game ->
              Games.update servers id { game with game_nb_players = nb_players } >>= fun () ->
              return (Some []))
              (function
                | Not_found -> return (Some [])
                | e -> fail e)
        end
        | Some (DELETE id) -> Games.remove servers id >> return (Some [])
        | Some LIST ->
            Games.games servers >>= fun g -> return (Some [GAMES g])
    end)

  let main addr =
    let servers = Games.create 17 in
    let tcp = TCP.create_server addr in
    Lwt_react.E.map_p (remove_from servers) (TCP.leave_event tcp)
    TCP.establish_server
      ~close:(fun cli -> remove_from servers cli)
      addr
      (treat_client servers)
end

let server = Server.main

module Client = struct

  module TCP = Network.TCP.Make(Protocol.Meta.Client)

  let add srv ~addr ~name ~nb_players =
    TCP.send srv (ADD (addr, name, nb_players)) >>= fun () ->
    TCP.recv srv >>>= function
      | ADDED game_id -> some
        { game_id; game_addr = addr; game_name = name
        ; game_nb_players = nb_players }
      | _ -> assert false (* TODO: error !!!! *)

  let update srv ~id ~nb_players =
    TCP.send srv (UPDATE (id, nb_players))

  let delete srv ~id =
    TCP.send srv (DELETE id)

  let list_games srv =
    TCP.send srv LIST >>= fun () ->
    TCP.recv srv >>>= function
      | GAMES games -> some games
      | _ -> assert false (* TODO: error *)
end
