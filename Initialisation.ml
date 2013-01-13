open Protocol.Initialisation

module Server = struct

  module TCP = Network.TCP.Make(Protocol.Initialisation.Server)

  type 'a state =
    { mutable next_id : int
    ; map : Data.map
    ; params : params
    ; mutable nb_players : int
    ; players : (string, string * char) Hashtbl.t
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

  let treat_message state = function
    | HELLO (pseudo, versions) ->
        (* Protocol check *)
        if not (List.mem 1 versions) then begin
          Some [REJECTED ("Only version 1 of the protocol is supported by this"
            ^ " server.")]
        (* TODO: check that the pseudo is available *)
        (* TODO: check that there is only one player per connection (?) *)
        (* Are there still places available ? *)
        end else if nb_players state < max_players state then begin
          (* Compute ID *)
          let c = char_of_int state.next_id in
          let s = String.make 1 c in
          (* Broadcast join message *)
          state.broadcast (JOIN (pseudo, s, c)); (* TODO *)
          (* Update broadcast function to get broadcast messages *)
          (* Get JOIN to send back from other players *)
          let joins = Hashtbl.fold
            (fun o_id (_, o_pseudo, o_pos) l ->
              JOIN (o_pseudo, o_id, o_pos) :: l)
            state.players in
          (* Add player *)
          state.nb_players <- state.nb_players + 1;
          state.next_id <- state.next_id + 1;
          Hashtbl.add state.players s (client, pseudo, c);
          (* Send messages *)
          Some (OK (s, state.map, state.params) :: joins)
        end else begin
          Some [REJECTED "There is no room for an additional player."]
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
    let internal_stream, push = Lwt_stream.create () in
    let dup_stream = duplicate internal_stream in
    establish_server
      ~close:(fun cli -> remove cli)
      addr
      (fun client stream ->
      	let duped_stream = dup_stream () in
      	let client_stream = Lwt_stream.choose [stream, duped_stream] in
	Lwt_stream.flatten (treat_client client_stream))
        
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

(* Basically :
  There is a Lwt thread for the server, that do different checks to see if the
  game is full or stuff like that, and send the START message when feeling OK.
  The thread will then shut off the connections (allowing for the pending
  messages to be sent, though), wait the delay indicated in the parameters
  dictionary, then start the UPD game server. Yay!

  There also is a Lwt thread for each client, that wait for two things : JOIN
  messages from its client, or messages from the server to broadcast. When
  receiving a START message to broadcast, this thread closes the connection
  with its associated client and dies. I hope I can copy a push stream. If
  needed, I will have a stream per client and feed it back (simulated *outside*
  of state ?)
*)
