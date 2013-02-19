open Lwt
open Misc
open Protocol.Initialisation
open Protocol.Game

module R = Reorderable

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
    ; turns = R.create 17
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
