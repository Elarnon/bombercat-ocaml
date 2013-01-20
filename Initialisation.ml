open Protocol.Initialisation
open Misc
open Lwt

module Server = struct

  module Connection = Network.TCP.Make(Protocol.Initialisation.Server)

  type 'a state =
    (* Identifier sequence to make them unique *)
    { mutable next_id : int
    (* Set of available characters on the map *)
    ; available : (char, unit) Hashtbl.t
    (* The world map *)
    ; map : Data.map
    (* Game parameters *)
    ; params : params
    (* All registered players *)
    ; players : (string, string * char) Hashtbl.t
    ; mutable starting : (Unix.tm * int) option
    (* Called on game update *)
    ; updated : unit Lwt_condition.t
    ; meta : Meta.Client.Connection.t
    ; game : Protocol.Meta.game
    }

  let mk_state map params meta game =
    let available = Hashtbl.create 17 in
    Hashtbl.iter (fun k _ -> Hashtbl.add available k ()) map.Data.players;
    { next_id = 0
    ; available
    ; map
    ; params
    ; players = Hashtbl.create 17
    ; updated = Lwt_condition.create ()
    ; starting = None
    ; meta
    ; game
    }

  let nb_players { players; _ } = Hashtbl.length players

  let max_players { map ; _ } = Hashtbl.length map.Data.players

  (* Treat message as authentified user [cid] in state [state] *)
  let treat_message cid state = function
    | HELLO _ ->
        (Some cid, [REJECTED "There can be only one user per connection."])

  (* Treat end of connection as authentified user [cid] in state [state] *)
  let treat_end cid state () =
    begin try
      let _, chr = Hashtbl.find state.players cid in
      (* Remove player from world *)
      Hashtbl.remove state.players cid;
      (* Mark its position as available again *)
      Hashtbl.add state.available chr ();
      return ()
    with Not_found ->
      Lwt_log.fatal ("Client ID not found in state. Continuing could lead to " ^
      "severe inconsistencies, aborting everything. This is a programmer " ^
      "failure.") >>
      assert false
    end >>= fun () ->
    (* Broadcast quitting *)
    Lwt_condition.broadcast state.updated ();
    return ()

  exception Found of char

  let treat_anonymous_message state = function
    | HELLO (pseudo, versions) ->
        (* Protocol check *)
        if not (List.mem 1 versions) then 
          (None, [REJECTED ("Only version 1 of the protocol is supported by this"
            ^ " server.")])
        (* Are there still places available ? *)
        else if nb_players state >= max_players state then 
          (None, [REJECTED "There is no room for an additional player."])
        (* Is game starting ? *)
        else if state.starting <> None then
          (None, [REJECTED "Game is already starting. Sorry!"])
        else
          (* Check that the pseudo is available *)
          let duplicated =
            Hashtbl.fold
              (fun _ (o, _) v -> v || o = pseudo)
              state.players
              false in
          if duplicated then
            (None, [REJECTED "There is already a player with this pseudo."])
          else begin
            (* Compute ID *)
            let c = try
              Hashtbl.iter (fun k () -> raise (Found k)) state.available;
              (* Impossible: [nb_players state < max_players state] here *)
              Lwt.ignore_result (
                Lwt_log.fatal ("Unavailable ID while there shall still be room " ^
                "for player. This is a programmer failure. Aborting everything.")
              );
              assert false
            with Found k -> k in
            Hashtbl.remove state.available c;
            let s = string_of_int state.next_id in
            state.next_id <- state.next_id + 1;
            (* Get JOIN to send back from other players. Should be useless now,
             * investigate (TODO). *)
            let joins = Hashtbl.fold
              (fun o_id (o_pseudo, o_pos) l ->
                JOIN (o_pseudo, o_id, o_pos) :: l)
              state.players [] in
            Hashtbl.add state.players s (pseudo, c);
            (* Broadcast join message *)
            Lwt_condition.broadcast state.updated ();
            (* Send messages *)
            (Some s, (OK (s, state.map, state.params) :: joins))
          end

  let treat state stream =
    let out, push = Lwt_stream.create () in
    let my_players = Hashtbl.create 17 in
    let client_stream = Lwt_stream.map (fun x -> `Client x) stream
    and server_stream = Lwt_stream.flatten (Lwt_stream.from (fun () ->
      Lwt_condition.wait state.updated >>
      let startings =
        match state.starting with
        | None -> []
        | Some (x, y) -> [`Start (x, y)]
      in let joins =
        Hashtbl.fold
          (fun id (pseudo, pos) l ->
            try
              if Hashtbl.find my_players id <> (pseudo, pos) then
                `Quit id :: `Join (pseudo, id, pos) :: l
              else l
            with Not_found -> `Join (pseudo, id, pos) :: l)
          state.players
          startings
      in let quits =
         Hashtbl.fold
          (fun id (pseudo, pos) l ->
            if not (Hashtbl.mem state.players id) then
              `Quit id :: l
            else l)
          my_players
          joins
      in Hashtbl.clear my_players;
      Hashtbl.iter (fun k v -> Hashtbl.add my_players k v) state.players;
      return (Some quits))) in
    let real_stream = merge ~quit:true [ client_stream ; server_stream ] in
    let rec treat_input do_message do_end =
      Lwt_stream.get real_stream >>= function
        | Some (`Client msg) ->
            let cli, lst = do_message state msg in
            List.iter (fun e -> push (Some e)) lst;
            Lwt_condition.broadcast state.updated ();
            begin match cli with
              | Some s -> treat_input (treat_message s) (treat_end s)
              | None -> treat_input treat_anonymous_message (fun _ -> return)
            end
        | Some (`Start (x,y)) ->
            push (Some (START (x, y)));
            push None;
            return ()
        | Some (`Join (s, s', c)) ->
            push (Some (JOIN (s, s', c)));
            treat_input do_message do_end
        | Some (`Quit a) ->
            push (Some (QUIT a));
            treat_input do_message do_end
        | None -> push None; do_end state ()
    in
    Lwt.async (fun () -> treat_input treat_anonymous_message (fun _ -> return));
    out

  let rec handle_server server state =
    let cur_players = nb_players state in
    Lwt_condition.wait state.updated >>
    if nb_players state <> cur_players then
      Meta.Client.update
        state.meta
        ~id:state.game.Protocol.Meta.game_id
        ~nb_players:(max_players state - nb_players state);
    if nb_players state = max_players state then begin
      (* Ready to start ! *)
      Lwt_io.shutdown_server server;
      Meta.Client.delete state.meta ~id:state.game.Protocol.Meta.game_id;
      let (nanof, datef) = modf (Unix.gettimeofday ()) in
      let date = Unix.gmtime datef in
      let nano = int_of_float (nanof *. 10000.) in
      state.starting <- Some (date, nano);
      Lwt_condition.broadcast state.updated ();
      return (date, nano)
    end else
      handle_server server state

  let create addr meta =
    Unix.handle_unix_error (fun () ->
    (* Load the world, TODO ? *)
    Lwt_stream.to_string (Lwt_io.chars_of_file "world") >>= fun s ->
    let map = Data.map_of_string s in
    Meta.Client.add meta ~addr ~name:"CATSERV"
      ~nb_players:(Hashtbl.length map.Data.players) >>= function
        | None -> assert false (* TODO *)
        | Some game ->
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
            let state = mk_state map params meta game in
            let server = Connection.establish_server
              addr
              (fun client stream -> treat state stream)
            in handle_server server state
  ) ()

end

module Client = struct

  module Connection = Network.TCP.Make(Protocol.Initialisation.Client)

  type event =
    [ `Start of Unix.tm * int
    | `Join of string * string * char
    | `Quit of string
    | `Closed ]

  type t =
    Connection.t * Lwt_mutex.t * event Lwt_stream.t *
    [ `Rejected of string
    | `Ok of string * Data.map * Protocol.Initialisation.params ] Lwt_stream.t

  let connect addr =
    Connection.open_connection addr >>= fun co ->
    let stream, push = Lwt_stream.create () in
    let stream_hello, push_hello = Lwt_stream.create () in
    let rec loop () =
      Connection.recv co >>= function
        | None -> push_hello None; push None; return ()
        | Some (REJECTED s) -> push_hello @$ Some (`Rejected s); loop ()
        | Some (OK (s, m, p)) -> push_hello @$ Some (`Ok (s, m, p)); loop ()
        | Some (START (date, nano)) ->
            push @$ Some (`Start (date, nano)); loop ()
        | Some (JOIN (pseudo, id, pos)) ->
            push @$ Some (`Join (pseudo, id, pos)); loop ()
        | Some (QUIT id) ->
            push @$ Some (`Quit id); loop ()
    in
      Lwt.async loop;
      return (co, Lwt_mutex.create (), stream, stream_hello)

  let hello (srv, mut, _, stream) ~pseudo ~versions =
    Lwt_mutex.with_lock mut (fun () ->
      Connection.send srv (HELLO (pseudo, versions));
      Lwt_stream.get stream)

  let poll (_, _, stream, _) = Lwt_stream.get stream

end
