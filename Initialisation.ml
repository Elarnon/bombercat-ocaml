open Protocol.Initialisation

module Server = struct

  type 'a state =
    { mutable next_id : int
    ; map : Data.map
    ; params : params
    ; mutable nb_players : int
    ; players : (string, 'a Server.chan * string * char) Hashtbl.t
    }

  let mk_state map params =
    { next_id = int_of_char 'A'
    ; map
    ; params
    ; nb_players = 0
    ; players = Hashtbl.create 17
    }

  let nb_players { nb_players; _ } = nb_players

  let max_players { map ; _ } = Hashtbl.length map.Data.players

  let broadcast { players; _ } message =
    Hashtbl.iter
      (fun _ (chan, _, _) ->
        Server.write chan message)
      players

  let flush { players; _ } =
    Hashtbl.iter
      (fun _ (chan, _, _) ->
        Server.check_ops ~write:[chan] ();
        Server.flush chan)
      players

  let treat_message client state = function
    | HELLO (pseudo, versions) ->
        if not (List.mem 1 versions) then begin
          Server.write client (REJECTED ("Client doesn't support version 1 of "
          ^ "the protocol."))
        end else if nb_players state < max_players state then begin
          let c = char_of_int state.next_id in
          let s = String.make 1 c in
          Server.write client (OK (s, state.map, state.params));
          Hashtbl.add state.players s (client, pseudo, c);
          state.nb_players <- state.nb_players + 1;
          state.next_id <- state.next_id + 1;
          broadcast state (JOIN (pseudo, s, c))
        end else begin
          Server.write client (REJECTED "Too many clients.");
        end

  type ret = Disconnected | Alive

  let rec treat_client client state =
    let open Server in 
    Server.check_ops ~read:[client] ();
    match
      (try Server.read client
      with Protocol.ReadError -> prerr_endline "protocol error"; Nothing
       | Bencode.Format_error -> prerr_endline "bencode error"; EOF) with
       | Nothing -> Alive
       | EOF -> Disconnected
       | Message m -> treat_message client state m; treat_client client state

  let treat_all clients state =
    List.fold_left
      (fun nclients client ->
        match treat_client client state with
        (* TODO: remove corresponding games ? *)
        | Disconnected -> nclients (* TODO: remove from table *)
        | Alive -> client :: nclients)
      []
      clients

  let rec step serv clients infos =
    Server.check_ops ~accept:[serv] ~read:clients ~write:clients ();
    match Server.accept serv with
    | Some client -> (* Incoming connection *)
        step serv (client :: clients) infos
    | None ->
        let new_clients = treat_all clients infos in (* Don't forget deconnections *)
        List.iter Server.flush new_clients; (* Flush messages *)
        step serv new_clients infos

  let main addr =
    Unix.handle_unix_error (fun () ->
    let chan = Server.mk_server addr in
    let wstr =
      let s = ref "" in
      let f = open_in "world" in
      begin try while true do
        s:= !s ^ input_line f ^ "\n"
      done with End_of_file -> close_in f end;
      !s  in
    let map = Data.map_of_string wstr in
    let params = 
      { p_game_time = max_int - 1
      ; p_bomb_time = 4
      ; p_bomb_dist = 2
      ; p_map_width = map.Data.width
      ; p_map_height = map.Data.height
      ; p_turn_time = 250
      ; p_start_delay = 3000
      ; p_version = 1
      } in
    let state = mk_state map params in
    step chan [] state
    ) ()

end
