open Protocol.Initialisation
open Misc
open Lwt

module CSet = Set.Make(Char)

module Server = struct

  module TCP = Network.TCP.Make(Protocol.Initialisation.Server)

  type 'a state =
    { mutable next_id : int
    ; available : (char, unit) Hashtbl.t
    ; map : Data.map
    ; params : params
    ; mutable nb_players : int
    ; players : (string, string * char) Hashtbl.t
    ; broadcast : Protocol.Initialisation.server -> unit
    }

  let mk_state map params push =
    let available =  Hashtbl.create 17 in
    Hashtbl.iter (fun k _ -> Hashtbl.add available k ()) map.Data.players;
    { next_id = int_of_char 'A'
    ; available
    ; map
    ; params
    ; nb_players = 0
    ; players = Hashtbl.create 17
    ; broadcast = fun x -> push (Some x)
    }

  let nb_players { nb_players; _ } = nb_players

  let max_players { map ; _ } = Hashtbl.length map.Data.players

  let rec treat_message cid state = function
    | HELLO _ ->
        (Some cid, [REJECTED "Already a connection there. Open another connection."])
  and treat_end cid state () =
    (* TODO: catch Not_found *)
    let _, chr = Hashtbl.find state.players cid in
    state.broadcast (QUIT cid);
    Hashtbl.remove state.players cid;
    Hashtbl.add state.available chr ();
    state.nb_players <- state.nb_players - 1;
    (* TODO: send update to meta server *)
    return ()

  exception Found of char

  let treat_anonymous_message state = function
    | HELLO (pseudo, versions) ->
        (* Protocol check *)
        if not (List.mem 1 versions) then begin
          (None, [REJECTED ("Only version 1 of the protocol is supported by this"
            ^ " server.")])
        (* TODO: check that the pseudo is available *)
        (* TODO: check that there is only one player per connection (?) *)
        (* Are there still places available ? *)
        end else if nb_players state < max_players state then begin
          (* Compute ID *)
          let c = try
            Hashtbl.iter (fun k () -> raise (Found k)) state.available;
            failwith "Very bad. Inconsistency."
          with Found k -> k in
          let s = String.make 1 (char_of_int state.next_id) in
          (* Update broadcast function to get broadcast messages *)
          (* Get JOIN to send back from other players *)
          let joins = Hashtbl.fold
            (fun o_id (o_pseudo, o_pos) l ->
              JOIN (o_pseudo, o_id, o_pos) :: l)
            state.players [] in
          (* Broadcast join message *)
          state.broadcast (JOIN (pseudo, s, c));
          (* Add player *)
          state.nb_players <- state.nb_players + 1;
          state.next_id <- state.next_id + 1;
          Hashtbl.add state.players s (pseudo, c);
          (* Send messages *)
          (Some s, (OK (s, state.map, state.params) :: joins))
        end else begin
          (None, [REJECTED "There is no room for an additional player."])
        end

  let treat state stream =
    let out, push = Lwt_stream.create () in
    let rec treat_input do_message do_end =
      Lwt_stream.get stream >>= (function
        | Some msg -> 
            let cli, lst = do_message state msg in
            push (Some lst);
            begin match cli with
              | Some s -> treat_input (treat_message s) (treat_end s)
              | None -> treat_input treat_anonymous_message (fun _ -> return)
            end
        | None -> push None; do_end state ())
    in Lwt.ignore_result (treat_input treat_anonymous_message (fun _ -> return));
       out

  let main addr =
    Unix.handle_unix_error (fun () ->
    (* Load the world, TODO *)
    Lwt_stream.to_string (Lwt_io.chars_of_file "world") >>= fun s ->
    let map = Data.map_of_string s in
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
    (* Create a server stream and a duplicator function for each client *)
    let streams = Hashtbl.create 17 in
    let remove_stream cli = Hashtbl.remove streams cli in
    let add_stream cli =
      let stream, push = Lwt_stream.create () in
      Hashtbl.replace streams cli push;
      stream in
    let push msg =
      Hashtbl.iter (fun _ push -> push msg) streams in
    let state = mk_state map params push in
    TCP.establish_server
      ~close:(fun cli -> remove_stream cli; return ())
      addr
      (fun client stream ->
        let others_stream = add_stream client in
        let client_stream = Lwt_stream.flatten (treat state stream) in
        let with_stream s = Lwt_stream.peek s >>= fun x -> return (x, s) in
        Lwt_stream.from (fun () ->
          let from_client = with_stream client_stream
          and from_others = with_stream others_stream in
          from_client <?> from_others >>= fun (x, s) ->
          Lwt_stream.junk s >> return x))
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
