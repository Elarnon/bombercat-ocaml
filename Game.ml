open Lwt
open Misc
open Protocol.Initialisation
open Protocol.Game

type player_internal =
  { pi_pseudo : string
  ; pi_map_id : char
  ; mutable pi_reject : int
  ; mutable pi_sequence : int
  ; mutable pi_author : Network.addr option
  ; pi_commands : (int, client_command) Hashtbl.t
  ; pi_queue : [ `Nop of int | `Client of client_command ] Queue.t
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

let seq_window = 3

let max_commands = 5

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
        if rej = pi.pi_reject && seq > pi.pi_sequence && is_player st id &&
          check_author pi id from
        then
          (* TODO: assert found *)
          let cmds = pi.pi_commands in
          if Hashtbl.length cmds < seq_window then begin
            if seq > pi.pi_sequence + seq_window then begin
              (* We are out of sync! *)
              Queue.add (`Nop pi.pi_sequence) pi.pi_queue;
              Hashtbl.clear cmds;
              pi.pi_reject <- pi.pi_reject + 1;
              pi.pi_sequence <- pi.pi_sequence + 1;
            end else begin (* Normally adding *)
              let pos = match msg with | BOMB p | MOVE (p, _) -> p in
              if Data.is_pos_valid map pos then
              Hashtbl.replace cmds seq msg;
              begin try while Queue.length pi.pi_queue < max_commands do
                let cmd = hashtbl_take cmds (pi.pi_sequence + 1) in
                Queue.add (`Client cmd) pi.pi_queue;
                pi.pi_sequence <- pi.pi_sequence + 1;
              done with Not_found -> () end;
            end;
          end else (* Wow, we are too fast *)
            Lwt.async (fun () ->
              Lwt_log.warning "Ignoring COMMAND because queue is too big.")
      with Not_found -> () end
  | SYNC (id, turn) ->
      begin try
        let pi = Hashtbl.find players id in
        if turn_is_past st turn && is_player st id && validate_author pi id from
        then
          let plogs = discard turn @$ st.logs in
          let strings = all_servers_to_strings ~nops:id plogs in
          List.iter (fun str ->
            ignore (Network.UDP.sendto st.socket str from)) strings;
      with Not_found -> () end

let rec treat_input game =
  Network.UDP.recvfrom game.socket >>= function
    | None -> return ()
    | Some (str, addr) ->
        string_to_clients str >|= List.iter (treat_message game addr) >>
        treat_input game

let run_turn st =
  let nb_players = Hashtbl.length (Data.players st.map) in
  if not st.ended && nb_players <= 1 then begin
    let module X = struct exception Found of char end in
    st.ended <- true;
    let winner =
      try Hashtbl.iter (fun id -> raise (X.Found id)) (Data.players st.map); None
      with X.Found id -> Some (id_of_map st id)
    in GAMEOVER (st.turn, { winner })
  end else
    let tbl = Hashtbl.create 17 in
    Hashtbl.iter
      (fun id pi ->
        let cmds = pi.pi_queue in
        let actions = Stack.create () in
        let module X = struct exception Break end in
        let bomb = ref false
        and move = ref false in
        begin try while true do
          match Queue.peek cmds with
          | `Client (BOMB pos) ->
              if !bomb then begin (* a bomb was already put *)
                raise X.Break
              end else begin
                queue_junk cmds;
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
                queue_junk cmds;
                let pid = pi.pi_map_id in
                if Data.try_move st.map pid pos dir then begin
                  move := true;
                  Stack.push (CLIENT (MOVE (pos, dir))) actions;
                end (* else impossible to put bomb for some reason *)
              end
          | `Nop s ->
              queue_junk cmds;
              Stack.push (NOP s) actions
        done with Queue.Empty | X.Break -> () end;
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
      ; pi_sequence = -1
      ; pi_author = None
      ; pi_commands = Hashtbl.create 17
      ; pi_queue = Queue.create ()
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
  Lwt.async (fun () -> Data.display server.map);
  Lwt_unix.sleep ttime >> loop server
