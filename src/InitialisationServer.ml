open Protocol.Initialisation
open Misc
open Lwt

type game =
  { g_map : Data.map
  ; g_params : Protocol.Initialisation.params
  ; g_players : (string, string * char) Hashtbl.t
  ; g_start : Unix.tm * int
  ; g_name : string
  }

module Connection = Network.TCP.Make(Protocol.Initialisation.Server)

type state =
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
  (* Starting time *)
  ; mutable starting : (Unix.tm * int) option
  (* Called on game update *)
  ; updated : unit Lwt_condition.t
  (* Connection to the meta server *)
  ; meta : MetaClient.Connection.t
  (* Game parameters *)
  ; game : Protocol.Meta.game
  }

let mk_state map params meta game =
  let available = Hashtbl.create 17 in
  Data.iter_players (fun k _ -> Hashtbl.add available k ()) map;
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

let max_players { map ; _ } = Data.map_nb_players map

(* Treat message as authentified user [cid] in state [state] *)
let treat_message cid _state = function
  | HELLO _ ->
      (Some cid, [REJECTED "There can be only one user per connection."])
  | SPECTATOR _ -> (* TODO *)
      (Some cid, [REJECTED "You are already a player, fool."])

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
  | SPECTATOR _pseudo -> (* TODO *)
      (None, [REJECTED ("Spectators are not supported by this server.")])
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
          (* let joins = Hashtbl.fold
            (fun o_id (o_pseudo, o_pos) l ->
              JOIN (o_pseudo, o_id, o_pos) :: l)
            state.players [] in *)
          let joins = [] in
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
        (fun id _ l ->
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
    MetaClient.update
      state.meta
      ~id:state.game.Protocol.Meta.game_id
      ~nb_players:(nb_players state);
  if nb_players state = max_players state then begin
    (* Ready to start ! *)
    Lwt_io.shutdown_server server;
    MetaClient.delete state.meta ~id:state.game.Protocol.Meta.game_id;
    MetaClient.Connection.close state.meta;
    let (nanof, datef) = modf (Unix.gettimeofday ()) in
    let date = Unix.gmtime datef in
    let nano = int_of_float (nanof *. 10000.) in
    state.starting <- Some (date, nano);
    Lwt_condition.broadcast state.updated ();
    return { g_map = state.map
           ; g_params = state.params
           ; g_players = state.players
           ; g_start = (date, nano)
           ; g_name = state.game.Protocol.Meta.game_name
           }
  end else
    handle_server server state

let create addr meta =
  Unix.handle_unix_error (fun () ->
  (* Load the world *)
  Lwt_stream.to_string (Lwt_io.chars_of_file "world") >>= fun s ->
  let map = Data.map_of_string s in
  MetaClient.add meta ~addr ~name:"CATSERV"
    ~nb_players:(Data.map_nb_players map) >>= function
      | None -> return_none
      | Some game ->
          let params =
            { p_game_time = max_int - 1
            ; p_bomb_time = 6
            ; p_bomb_dist = 2
            ; p_map_width = Data.width map
            ; p_map_height = Data.height map
            ; p_turn_time = 200
            ; p_start_delay = 3000
            ; p_version = 1
            } in
          let state = mk_state map params meta game in
          let server = Connection.establish_server
            addr
            (fun _client stream -> treat state stream)
          in handle_server server state >|= fun x -> Some x
) ()
