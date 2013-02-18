open Lwt
open Misc
open Protocol.Initialisation
open Protocol.Game

module R = Reorderable

let window = 1

let max_size = 2


module Server = struct

  type player_internal =
    { pi_pseudo : string
    ; pi_map_id : char
    ; mutable pi_reject : int
    ; mutable pi_author : Network.addr option
    ; pi_commands : [ `Nop of int | `Client of client_command ] R.t
    }

  type t =
    { params : Protocol.Initialisation.params
    ; map : Data.map
    ; players : (string, player_internal) Hashtbl.t
    ; reverse : (char, string) Hashtbl.t
    ; mutable turn : int
    ; mutable logs : server list
    ; mutable ended : bool
    ; socket : Network.UDP.t
    }

  (* Turn checking *)
  let turn_is_past { turn; _ } t = t >= 0 && t < turn

  let turn_is_current { turn; _ } t = t = turn

  let turn_is_next { params; turn; _ } t =
    t < params.p_game_time && t = turn + 1

  let turn_is_future { params; turn; _ } t =
    t >= turn && t < params.p_game_time

  (* Player checking *)
  let is_player { players; _ } id = Hashtbl.mem players id

  let id_of_map { reverse; _ } mid = Hashtbl.find reverse mid

  let check_author pi id from =
    Some from = pi.pi_author

  let validate_author pi id from =
    match pi.pi_author with
    | Some author when author = from -> true
    | Some _ -> false
    | None -> pi.pi_author <- Some from; true

  let treat_message ({ map; players; _ } as st) from = function
    | COMMAND (id, seq, rej, msg) ->
        begin try
          let pi = Hashtbl.find players id in
          if rej = pi.pi_reject && is_player st id && check_author pi id from
          then
            let pos = match msg with | BOMB p | MOVE (p, _) -> p in
            if Data.is_pos_valid map pos then
              if not (R.add pi.pi_commands seq (`Client msg)) then begin
                (* Out of sync! *)
                R.interrupt ~notify:(fun x -> `Nop x) pi.pi_commands;
                pi.pi_reject <- pi.pi_reject + 1
              end
        with Not_found -> () end
    | SYNC (id, turn) ->
        begin try
          Lwt.async (fun () -> Lwt_log.debug ("SYNC received (" ^ id ^ ")"));
          Lwt.async (fun () ->
            let s = ref "" in
            Hashtbl.iter (fun k _ -> s := !s ^ ", " ^ k) players;
            Lwt_log.debug ("Valid players: " ^ !s));
          let pi = Hashtbl.find players id in
          if turn_is_past st turn && is_player st id && validate_author pi id from
          then begin
            Lwt.async (fun () -> Lwt_log.debug "SYNC treated");
            let plogs = discard turn @$ st.logs in
            let strings = all_servers_to_strings ~nops:id plogs in
            List.iter (fun str ->
              ignore (Network.UDP.sendto st.socket str from)) strings;
          end
        with Not_found -> () end

  let rec treat_input game =
    Network.UDP.recvfrom game.socket >>= function
      | None -> return ()
      | Some (str, addr) ->
          let clients = string_to_clients str in
          List.iter (treat_message game addr) clients;
          treat_input game

  let run_turn st =
    let nb_players = Data.map_nb_players st.map in
    if not st.ended && nb_players <= 1 then begin
      st.ended <- true;
      let winner = match Data.map_choose st.map with
        | Some id -> Some (id_of_map st id)
        | None -> None
      in GAMEOVER (st.turn, { winner })
    end else
      let tbl = Hashtbl.create 17 in
      Hashtbl.iter
        (fun id pi ->
          let cmds = pi.pi_commands in
          let actions = Stack.create () in
          let module X = struct exception Break end in
          let bomb = ref false
          and move = ref false in
          begin try while true do
            match R.peek cmds with
            | `Client (BOMB pos) ->
                if !bomb then begin (* a bomb was already put *)
                  raise X.Break
                end else begin
                  R.junk cmds;
                  let pid = pi.pi_map_id
                  and btime = st.params.p_bomb_time
                  and bdist = st.params.p_bomb_dist in
                  if Data.try_bomb st.map pid pos btime bdist then begin
                    bomb := true;
                    Stack.push (CLIENT (BOMB pos)) actions;
                  end (* else impossible to put bomb for some reason *)
                end
            | `Client (MOVE (pos, dir)) ->
                if !move then (* A move has already been issued *)
                  raise X.Break
                else begin
                  R.junk cmds;
                  let pid = pi.pi_map_id in
                  if Data.try_move st.map pid pos dir then begin
                    move := true;
                    Stack.push (CLIENT (MOVE (pos, dir))) actions;
                  end (* else impossible to put bomb for some reason *)
                end
            | `Nop s ->
                R.junk cmds;
                Stack.push (NOP s) actions
          done with R.Empty | X.Break -> () end;
          Hashtbl.add tbl id actions)
        st.players;
      List.iter (fun p -> Stack.push DEAD (Hashtbl.find tbl (id_of_map st p)))
                (Data.decrease_timers st.map);
      let smap = Hashtbl.fold (fun k v -> Smap.add k (dump_stack v)) tbl Smap.empty
      in let res = TURN (st.turn, smap) in
      st.turn <- st.turn + 1;
      res

  let rec loop server =
    let run () =
      let msg = run_turn server in
      server.logs <- msg :: server.logs;
      let last_logs = firsts 3 server.logs in
      Hashtbl.iter (fun id { pi_author; _ } ->
        match pi_author with
        | None -> ()
        | Some addr ->
            let str, _ = most_servers_to_string ~nops:id last_logs in
            ignore (Network.UDP.sendto server.socket str addr)) server.players;
      return_unit
    in let ttime = float_of_int server.params.p_turn_time /. 1000. in
    Lwt.join [ Lwt_unix.sleep ttime; run () ] >>
    if server.ended then return () else loop server

  let main addr game =
    let open Initialisation in
    let players = Hashtbl.create 17 in
    let reverse = Hashtbl.create 17 in
    Hashtbl.iter (fun id (pseudo, map_id) ->
      Hashtbl.add reverse map_id id;
      Hashtbl.add players id
        { pi_pseudo = pseudo
        ; pi_map_id = map_id
        ; pi_reject = 0
        ; pi_author = None
        ; pi_commands = R.create ~window ~max_size 17
        }) game.g_players;
    let socket = Network.UDP.create ~addr () in
    let server =
      { params = game.g_params
      ; map = game.g_map
      ; players
      ; reverse
      ; turn = 0
      ; logs = []
      ; ended = false
      ; socket
      } in
    let ttime = float_of_int server.params.p_turn_time /. 1000. in
    Lwt.async (fun () -> treat_input server);
    (* Lwt.async (fun () -> Data.display server.map); *)
    Lwt_unix.sleep ttime >> loop server
end

module Client = struct

  type t =
    { params : Protocol.Initialisation.params
    ; map : Data.map
    ; players : (string, (string * char)) Hashtbl.t
    ; ident : string
    ; pseudo : string
    ; map_id : char
    ; display : Display.t
    ; mutable reject : int
    ; mutable sequence : int
    ; mutable logs : Protocol.Game.client list
    ; socket : Network.UDP.t
    ; server : Network.addr
    ; turns : Protocol.Game.server R.t
    ; mutable last_sync : float
    ; redraw : int Lwt_condition.t
    }

  let resync t =
    let tout = float_of_int (t.params.p_turn_time) /. 4000. in
    let now = Unix.gettimeofday () in
    if now -. t.last_sync > tout then begin
      t.last_sync <- now;
      let strs = all_clients_to_strings [ SYNC (t.ident, R.last_id t.turns) ] in
      List.iter (fun str ->
        ignore (Network.UDP.sendto t.socket str t.server)) strs
    end

  let treat_message t turn =
    let id = match turn with | TURN (id, _) | GAMEOVER (id, _) -> id in
    if not (R.add t.turns id turn) then (* OOS *)
      resync t

  let rec treat_input t =
    try_lwt
      let tout = float_of_int (t.params.p_turn_time) /. 1000. *. 4. in
      Lwt_unix.with_timeout tout (fun () -> Network.UDP.recvfrom t.socket)
      >>= function
        | None -> Display.quit t.display
        | Some (str, addr) when addr = t.server ->
            let servers = string_to_servers str in
            Lwt_list.iter_s (fun m -> treat_message t m; Lwt_main.yield ())
            servers >>
            treat_input t
        | _ -> treat_input t
    with Lwt_unix.Timeout -> resync t; treat_input t

  let resend_commands t =
    let str, _ = most_clients_to_string (firsts 5 t.logs) in
    ignore (Network.UDP.sendto t.socket str t.server);
    return_unit

  let send_command t command =
    let res = COMMAND (t.ident, t.sequence, t.reject, command) in
    t.logs <- res :: t.logs;
    t.sequence <- t.sequence + 1;
    resend_commands t

  let rec treat_commands t =
    Display.input t.display >>= function
      | None -> Display.quit t.display
      | Some command -> send_command t command >> treat_commands t

  let rec update_map t =
    let tout = float_of_int t.params.p_turn_time /. 1000. *. 0.5 in
    Lwt_unix.sleep tout >>= fun () ->
    resend_commands t >>
    let module X = struct exception Win of string option end in
    begin try while true do
      match R.take t.turns with
      | TURN (tid, actions) ->
          Smap.iter (fun ident actions ->
            try
            let map_id = snd @$ Hashtbl.find t.players ident in
            List.iter (function
              | CLIENT (MOVE (pos, dir)) ->
                  ignore (Data.try_move t.map map_id pos dir)
              | CLIENT (BOMB pos) ->
                  let btime = t.params.p_bomb_time
                  and bdist = t.params.p_bomb_dist in
                  ignore (Data.try_bomb t.map map_id pos btime bdist)
              | NOP _ -> t.sequence <- 0; t.reject <- t.reject + 1
              | DEAD -> ()) actions
            with Not_found -> (* TODO log *) ()) actions;
          ignore (Data.decrease_timers t.map);
          Display.update t.display tid
      | GAMEOVER (_, { winner }) ->
         raise (X.Win winner)
    done; return_none with
      | R.Empty -> update_map t
      | X.Win winner -> return (Some winner) end

  let main dcreate addr map params players ident =
    let open Initialisation in
    let map_id = snd @$ Hashtbl.find players ident in
    lwt display = dcreate map params map_id in
    let t =
      { params
      ; map
      ; display
      ; players
      ; ident
      ; pseudo = fst @$ Hashtbl.find players ident
      ; map_id
      ; reject = 0
      ; sequence = 0
      ; logs = []
      ; socket = Network.UDP.create ()
      ; server = addr
      ; turns = R.create ~window:5 ~max_size:3 17
      ; last_sync = 0.0
      ; redraw = Lwt_condition.create ()
      } in
    Display.update display 0;
    resync t;
    Lwt.pick [
     update_map t;
     treat_commands t >> return_none;
     treat_input t >> return_none] >>= function
       | None -> print_endline "unexpected end of game"; return_unit
       | Some None -> print_endline "no winner"; return_unit
       | Some (Some winner) -> print_endline (winner ^ " wins!"); return_unit

end
