open Lwt
open Misc
open LTerm_geom
open Protocol.Meta

let port = ref 22222
let address = ref "127.0.0.1"

let spec =
  [ "--port", Arg.Set_int port, " TCP port of the meta server [22222]"
  ; "--address", Arg.Set_string address, " IP address of the meta server [127.0.0.1]"
  ]

let anon _ = ()

let usage = "usage: " ^ Sys.argv.(0) ^ " [--port <meta port>] "
            ^ "[--address <meta address>]"

let left_key lst key =
  List.fold_left
    (fun acc { game_id; _ } ->
      if key = None || Some game_id < key then
        Some game_id
      else None)
    None
    lst

let right_key lst key =
  let module X = struct exception Found of int end in
  try 
    List.iter
      (fun { game_id; _ } -> if Some game_id > key then raise(X.Found game_id))
      lst;
    None
  with X.Found k -> Some k

let rec loop ui current lst =
  let open LTerm_event in
  let open LTerm_key in
  LTerm_ui.wait ui >>= function
    | Key { code = Up } ->
        begin match left_key !lst !current with
        | None -> ()
        | Some k -> current := Some k end;
        LTerm_ui.draw ui;
        loop ui current lst
    | Key { code = Down } ->
        begin match right_key !lst !current with
        | None -> ()
        | Some k -> current := Some k end;
        LTerm_ui.draw ui;
        loop ui current lst
    | Key { code = Enter } ->
        begin match !current with
        | None -> loop ui current lst
        | Some id ->
            begin try
              return @$
                Some (List.find (fun { game_id; _ } -> game_id = id) !lst)
            with Not_found -> LTerm_ui.draw ui end
        end
    | Key { code = Escape } ->
        return None
    | ev -> loop ui current lst

let draw current games ui matrix =
  let module D = LTerm_draw in
  let size = LTerm_ui.size ui in
  let ctx = D.context matrix size in
  let cols = size.cols in
  let len = cols / 3 in
  D.clear ctx;
  let names_ctx =
    D.sub ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = len }
  and ip_ctx =
    D.sub ctx { row1 = 0; col1 = len; row2 = size.rows; col2 = 2*len }
  and players_ctx =
    D.sub ctx { row1 = 0; col1 = 2 * len; row2 = size.rows; col2 = size.cols }
  in
  D.draw_hline ctx 1 0 size.cols D.Heavy;
  D.draw_vline ip_ctx 0 0 size.rows D.Light;
  D.draw_vline players_ctx 0 0 size.rows D.Light;
  D.draw_string_aligned names_ctx 0 H_align_center "Name";
  D.draw_string_aligned ip_ctx 0 H_align_center "IP address";
  D.draw_string_aligned players_ctx 0 H_align_center "Players";
  List.iteri
    (fun i { game_name; game_addr; game_nb_players; game_max_players; game_id
           ; _ } ->
      let ip, port = Network.raw_addr game_addr in
      let ip = ip ^ ":" ^ string_of_int port in
      let players =
        string_of_int game_nb_players ^ "/" ^ string_of_int game_max_players in
      D.draw_string_aligned names_ctx (i + 2) H_align_center game_name;
      D.draw_string_aligned ip_ctx (i + 2) H_align_center ip;
      D.draw_string_aligned players_ctx (i + 2) H_align_center players;
      if Some game_id = current then
        let lctx =
          D.sub ctx { row1 = i + 2; row2 = i + 3; col1 = 0; col2 = size.cols }
        in D.fill_style lctx LTerm_style.({ none with reverse = Some true }))
    games

let _ = Lwt_main.run begin Lwt_unix.handle_unix_error (fun () ->
  Arg.parse
    (Arg.align spec)
    anon
    usage;
  (* TODO: catch Not_found *)
  try_lwt
    lwt addr = Network.mk_addr ~port:!port !address in
    lwt co = Meta.Client.Connection.open_connection addr in
    lwt term = Lazy.force LTerm.stdout in

    let current = ref None in
    let games = ref [] in
    let rec update_games ui =
      Meta.Client.list_games co >>= function
        | None -> return None
        | Some gs ->
            games :=
              List.sort
                (fun { game_id; _ } {game_id = id; _ } -> compare game_id id)
                gs;
            LTerm_ui.draw ui;
            Lwt_unix.sleep 0.2 >> update_games ui in
    lwt ui =
      LTerm_ui.create term (fun ui matrix -> draw !current !games ui matrix) in
    lwt game = try_lwt
      Lwt.pick
        [ loop ui current games
        ; update_games ui ]
      finally
        LTerm_ui.quit ui
    in match game with
    | None -> return ()
    | Some game -> return () (* start game *)
  with Not_found ->
    Lwt_log.fatal "Unable to connect to the server." >> exit 2
) () end
