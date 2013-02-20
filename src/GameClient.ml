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
  ; display : Display.Game.t
  ; mutable reject : int
  ; mutable sequence : int
  ; mutable pending : Protocol.Game.client list
  ; socket : Network.UDP.t
  ; server : Network.addr
  ; turns : Protocol.Game.server R.t
  ; mutable last_sync : float
  }

(* let treating t = function
  | BOMB pos ->
      List.fold_left
        (fun (ok, l) b ->
          if ok then b :: l
          else match b with
          | BOMB p' when pos = p' -> (true, l)
          | BOMB _ -> cancel b; (true, l)
          | MOVE _ -> (true, b :: l)) *)

let resend_commands t =
  let cmds = firsts 5 t.pending in
  if cmds <> [] then begin
    let str, n = most_clients_to_string cmds in
    ignore (Network.UDP.sendto t.socket str t.server)
  end;
  return_unit

let send_command t command =
  let res = COMMAND (t.ident, t.sequence, t.reject, command) in
  t.pending <- res :: t.pending;
  t.sequence <- t.sequence + 1;
  resend_commands t

let resync t =
  let tout = float_of_int (t.params.p_turn_time) /. 1000. in
  let now = Unix.gettimeofday () in
  (* Do not send a SYNC message if one was sent less than one turn earlier. *)
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
      | None ->
          return ()
      | Some (str, addr) when addr = t.server ->
          let servers = string_to_servers str in
          Lwt_list.iter_s (fun m -> treat_message t m; Lwt_main.yield ())
          servers >>
          treat_input t
      | _ -> treat_input t
  with Lwt_unix.Timeout -> resync t; treat_input t

let rec treat_commands t =
  Display.Game.input t.display >>= function
    | None -> return ()
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
                (* if map_id = t.map_id then
                  treating t (MOVE (pos, dir)); *)
                ignore (Data.try_move t.map map_id pos dir)
            | CLIENT (BOMB pos) ->
                (* if map_id = t.map_id then
                  treating t (BOMB pos); *)
                let btime = t.params.p_bomb_time
                and bdist = t.params.p_bomb_dist in
                ignore (Data.try_bomb t.map map_id pos btime bdist)
            | NOP _ -> t.sequence <- 0; t.reject <- t.reject + 1
            | DEAD -> ()) actions
          with Not_found -> (* TODO log *) ()) actions;
        ignore (Data.decrease_timers t.map);
        Display.Game.update t.display tid
    | GAMEOVER (_, { winner }) ->
       raise (X.Win winner)
  done; return_none with
    | R.Empty -> update_map t
    | X.Win winner -> return (Some winner) end

let main display addr map params players ident =
  let map_id = snd @$ Hashtbl.find players ident in
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
    ; pending = []
    ; socket = Network.UDP.create ()
    ; server = addr
    ; turns = R.create ()
    ; last_sync = 0.0
    } in
  Display.Game.update display 0;
  resync t;
  Lwt.pick [
   update_map t;
   treat_commands t >> return_none;
   treat_input t >> return_none] >>= function
     | None -> print_endline "unexpected end of game"; return_unit
     | Some None -> print_endline "no winner"; return_unit
     | Some (Some winner) -> print_endline (winner ^ " wins!"); return_unit
