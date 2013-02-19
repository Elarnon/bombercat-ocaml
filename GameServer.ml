open Lwt
open Misc
open Protocol.Initialisation
open Protocol.Game

module R = Reorderable

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

let window = 1

let max_size = 2

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

let check_author pi from =
  Some from = pi.pi_author

let validate_author pi from =
  match pi.pi_author with
  | Some author when author = from -> true
  | Some _ -> false
  | None -> pi.pi_author <- Some from; true

let treat_message ({ map; players; _ } as st) from = function
  | COMMAND (id, seq, rej, msg) ->
      begin try
        let pi = Hashtbl.find players id in
        (* TODO: verify that a rogue player can't forge commands *)
        if rej = pi.pi_reject && is_player st id && check_author pi from
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
        if turn_is_past st turn && is_player st id && validate_author pi from
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
  let open Initialisation.Server in
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
