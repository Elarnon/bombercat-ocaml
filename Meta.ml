open Lwt
  
open Protocol.Meta
  
open Misc
  
module S = Lwt_stream
  
module Server =
  struct
    type server = Lwt_io.server
    
    module Connection = Network.TCP.Make(Protocol.Meta.Server)
      
    (* Internal structure to represent a table of games *)
    module Games =
      struct
        type ('a, 'b) t =
          { tbl : ('a, 'b) Hashtbl.t; mutable next_id : int;
            ids : (Connection.client, 'a list) Hashtbl.t
          }
        
        let create n =
          { tbl = Hashtbl.create n; next_id = 1; ids = Hashtbl.create n; }
          
        let add servers client id game =
          (Hashtbl.add servers.tbl id game;
           let client_ids =
             try Hashtbl.find servers.ids client with | Not_found -> []
           in
             (Hashtbl.replace servers.ids client (id :: client_ids);
              return ()))
          
        let find servers id = wrap2 Hashtbl.find servers.tbl id
          
        let games servers =
          wrap3 Hashtbl.fold (fun _ v l -> v :: l) servers.tbl []
          
        let update servers id game =
          wrap3 Hashtbl.replace servers.tbl id game
          
        let remove servers id = wrap2 Hashtbl.remove servers.tbl id
          
        let next_id servers =
          (servers.next_id <- servers.next_id + 1; return servers.next_id)
          
        let take_ids servers client =
          Lwt.catch
            (fun () ->
               let v = Hashtbl.find servers.ids client
               in (Hashtbl.remove servers.ids client; return v))
            (function | Not_found -> return [] | exn -> Lwt.fail exn)
          
        let remove_from servers client =
          let __pa_lwt_0 = take_ids servers client
          in
            Lwt.bind __pa_lwt_0
              (fun ids -> Lwt_list.iter_p (remove servers) ids)
          
      end
      
    let treat_client servers client stream =
      S.flatten
        (S.from
           (fun () ->
              (S.get stream) >>=
                (function
                 | None -> return None
                 | Some (ADD (addr, name, nb_players)) ->
                     let __pa_lwt_0 = Games.next_id servers
                     in
                       Lwt.bind __pa_lwt_0
                         (fun id ->
                            let game =
                              {
                                game_id = id;
                                game_addr = addr;
                                game_name = name;
                                game_nb_players = 0;
                                game_max_players = nb_players;
                              }
                            in
                              Lwt.bind (Games.add servers client id game)
                                (fun _ -> return (Some [ ADDED id ])))
                 | Some (UPDATE (id, game_nb_players)) ->
                     Lwt.catch
                       (fun () ->
                          let __pa_lwt_0 = Games.find servers id
                          in
                            Lwt.bind __pa_lwt_0
                              (fun game ->
                                 Lwt.bind
                                   (Games.update servers id
                                      {
                                        (game)
                                        with
                                        game_nb_players = game_nb_players;
                                      })
                                   (fun _ -> return (Some []))))
                       (function
                        | Not_found -> return (Some [])
                        | exn -> Lwt.fail exn)
                 | Some (DELETE id) ->
                     Lwt.bind (Games.remove servers id)
                       (fun _ -> return (Some []))
                 | Some LIST ->
                     (Games.games servers) >|= (fun g -> Some [ GAMES g ]))))
      
    let create addr =
      let servers = Games.create 17
      in
        Connection.establish_server
          ~close: (fun cli -> Games.remove_from servers cli) addr
          (treat_client servers)
      
    let shutdown server = Lwt_io.shutdown_server server
      
  end
  
(* TODO, close everything *)
module Client =
  struct
    module Connection = Network.TCP.Make(Protocol.Meta.Client)
      
    let add srv ~addr ~name ~nb_players =
      (Connection.send srv (ADD (addr, name, nb_players));
       (Connection.recv srv) >>=
         (function
          | Some (ADDED game_id) ->
              return @$
                (Some
                   {
                     game_id = game_id;
                     game_addr = addr;
                     game_name = name;
                     game_nb_players = 0;
                     game_max_players = nb_players;
                   })
          | Some _ -> assert false
          | (* TODO *) None -> return_none))
      
    let update srv ~id ~nb_players =
      Connection.send srv (UPDATE (id, nb_players))
      
    let delete srv ~id = Connection.send srv (DELETE id)
      
    let list_games srv =
      (Connection.send srv LIST;
       (Connection.recv srv) >>=
         (function
          | Some (GAMES games) -> return @$ (Some games)
          | Some _ -> assert false
          | (* TODO *) None -> return_none))
      
  end
  

