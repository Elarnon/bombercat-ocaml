open Lwt
open Protocol.Meta
open Misc
module S = Lwt_stream

module Server = struct

  module TCP = Network.TCP.Make(Protocol.Meta.Server)

  module Games = struct

    type ('a, 'b) t =
      { tbl : ('a, 'b) Hashtbl.t
      ; lock : Lwt_mutex.t
      ; mutable next_id : int
      ; lock_id : Lwt_mutex.t
      ; ids : (TCP.client, 'a list) Hashtbl.t
      ; lock_ids : Lwt_mutex.t
      }

    let create n =
      { tbl = Hashtbl.create n
      ; lock = Lwt_mutex.create ()
      ; next_id = 1
      ; lock_id = Lwt_mutex.create ()
      ; ids = Hashtbl.create 17
      ; lock_ids = Lwt_mutex.create ()
      }

    let add servers client id game =
      let adder () =
        return (Hashtbl.add servers.tbl id game)
      in Lwt.ignore_result (Lwt_mutex.with_lock servers.lock adder);
      Lwt.ignore_result (Lwt_mutex.with_lock servers.lock_ids
        (fun () -> return (
          let v =
            try Hashtbl.find servers.ids client with Not_found -> []
          in Hashtbl.replace servers.ids client (id :: v))))

    let find servers id =
      let finder () =
        try
          return (Hashtbl.find servers.tbl id)
        with Not_found -> fail Not_found
      in Lwt_mutex.with_lock servers.lock finder

    let games servers =
      let getter () =
        return (Hashtbl.fold (fun _ v l -> v :: l) servers.tbl [])
      in Lwt_mutex.with_lock servers.lock getter

    let update servers id game =
      let updater () =
        return (Hashtbl.replace servers.tbl id game)
      in Lwt.ignore_result (Lwt_mutex.with_lock servers.lock updater)

    let remove servers id =
      let remover () =
        return (Hashtbl.remove servers.tbl id)
      in Lwt.ignore_result (Lwt_mutex.with_lock servers.lock remover)

    let next_id servers =
      Lwt_mutex.with_lock servers.lock_id (fun () ->
        servers.next_id <- servers.next_id + 1;
        return servers.next_id)

    let take_ids servers client =
      Lwt_mutex.with_lock servers.lock_ids (fun () ->
        try
          let v = Hashtbl.find servers.ids client
          in Hashtbl.remove servers.ids client;
          return v
        with Not_found -> return [])

  end

  let remove_from servers client =
    Games.take_ids servers client >>= fun ids ->
    Lwt_list.iter_p (fun id -> Games.remove servers id; return ()) ids

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
            Games.add servers client id game;
            return (Some [ADDED id])
        | Some (UPDATE (id, nb_players)) -> begin
            catch (fun () ->
              Games.find servers id >>= fun game ->
              Games.update servers id { game with game_nb_players = nb_players };
              return (Some []))
              (function
                | Not_found -> return (Some [])
                | e -> fail e)
        end
        | Some (DELETE id) -> Games.remove servers id; return (Some [])
        | Some LIST ->
            Games.games servers >>= fun g -> return (Some [GAMES g])
    end)

  let main addr =
    let servers = Games.create 17 in
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
