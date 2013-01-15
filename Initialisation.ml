open Protocol.Initialisation
open Misc
open Lwt

module CSet = Set.Make(Char)

module Server = struct

  module TCP = Network.TCP.Make(Protocol.Initialisation.Server)

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
    ; update : unit Lwt_condition.t
    (* Called on game start *)
    ; start : (Unix.tm * int) Lwt_condition.t
    (* Called on player join *)
    ; joins : (string * string * char) Lwt_condition.t
    (* Called on player quit *)
    ; quit : string Lwt_condition.t
    }

  let mk_state map params push =
    let available = Hashtbl.create 17 in
    Hashtbl.iter (fun k _ -> Hashtbl.add available k ()) map.Data.players;
    { next_id = 0
    ; available
    ; map
    ; params
    ; players = Hashtbl.create 17
    ; update = Lwt_condition.create ()
    ; start = Lwt_condition.create ()
    ; joins = Lwt_condition.create ()
    ; quit = Lwt_condition.create ()
    }

  let nb_players { players; _ } = Hashtbl.length players

  let max_players { map ; _ } = Hashtbl.length map.Data.players

  (* Treat message as authentified user [cid] in state [state] *)
  let treat_message cid state = function
    | HELLO _ ->
        (Some cid, [REJECTED "There can be only one user per connection."])

  (* Treat end of TCP connection as authentified user [cid] in state [state] *)
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
    Lwt_condition.broadcast state.quit cid;
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
                "player. This is a programmer failure. Aborting everything.")
              );
              assert false
            with Found k -> k in
            Hashtbl.remove state.available c;
            let s = string_of_int state.next_id in
            state.next_id <- state.next_id + 1;
            (* Get JOIN to send back from other players *)
            let joins = Hashtbl.fold
              (fun o_id (o_pseudo, o_pos) l ->
                JOIN (o_pseudo, o_id, o_pos) :: l)
              state.players [] in
            Hashtbl.add state.players s (pseudo, c);
            (* Broadcast join message *)
            Lwt_condition.broadcast state.joins (pseudo, s, c);
            (* Send messages *)
            (Some s, (OK (s, state.map, state.params) :: joins))
          end

  let treat state stream =
    let out, push = Lwt_stream.create () in
    let client_stream = Lwt_stream.map (fun x -> `Client x) stream
    and server_stream = Lwt_stream.from (fun () ->
      let start_ = Lwt_condition.wait state.start >|= fun (x,y) -> `Start (x,y)
      and join_  = Lwt_condition.wait state.joins >|= fun x -> `Join x
      and quit_  = Lwt_condition.wait state.quit  >|= fun x -> `Quit x
      in Lwt.choose [ start_; join_ ; quit_ ] >|= fun x -> Some x) in
    let real_stream = merge ~quit:true [ client_stream ; server_stream ] in
    let rec treat_input do_message do_end =
      Lwt_stream.get real_stream >>= function
        | Some (`Client msg) ->
            let cli, lst = do_message state msg in
            List.iter (fun e -> push (Some e)) lst;
            Lwt_condition.broadcast state.update ();
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
    Lwt_condition.wait state.update >>
    if nb_players state = max_players state then begin
      (* Ready to start ! *)
      (* Lwt_io.shutdown_server server; *)
      let (nanof, datef) = modf (Unix.gettimeofday ()) in
      let date = Unix.gmtime datef in
      let nano = int_of_float (nanof *. 10000.) in
      Lwt_condition.broadcast state.start (date, nano);
      return (date, nano)
    end else
      handle_server server state

  let main addr =
    Unix.handle_unix_error (fun () ->
    (* Load the world, TODO ? *)
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
    let server = TCP.establish_server
      ~close:(fun cli -> remove_stream cli; return ())
      addr
      (fun client stream -> treat state stream)
    in handle_server server state
  ) ()

end
