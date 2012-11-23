open Lwt
open Gobject.Data

let cols = new GTree.column_list
let col_name = cols#add string
let col_ip = cols#add string
let col_players = cols#add int

let mk_model =
  let store = GTree.list_store cols in
  let update games =
    let open Protocol.Meta in
    store#clear ();
    List.iter
      (fun { game_name; game_addr; game_nb_players; _ } ->
        let row = store#append () in
        let (ip, port) = Network.raw_addr game_addr in
        store#set ~row ~column:col_name game_name;
        store#set ~row ~column:col_players game_nb_players;
        store#set ~row ~column:col_ip (ip ^ ":" ^ string_of_int port)
      )
      games
  in (store, update)

let create_view ~model ~packing () =
  let view = GTree.view ~model ~packing () in

  (* Column #1: col_name is string column *)
  let col = GTree.view_column ~title:"Name"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_name]) () in
  ignore (view#append_column col);

  (* Column #2: col_age is int column *)
  let col = GTree.view_column ~title:"IP address"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_ip]) () in
  ignore (view#append_column col);

  let col = GTree.view_column ~title:"Number of players"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_players]) () in
  ignore (view#append_column col);

  view

let () = Lwt_main.run (
      (* Initializes GTK. *)
      ignore (GMain.init ());

      (* Install Lwt<->Glib integration. *)
      Lwt_glib.install ();

      (* Thread which is wakeup when the main window is closed. *)
      let waiter, wakener = Lwt.wait () in

      (* Create a window. *)
      let window = GWindow.window () in

      let (model, update) = mk_model in
      Network.parse_addr "127.0.0.1:12345" >>= fun addr ->
      Meta.Client.TCP.open_connection addr >>= fun co ->
      let rec poll () =
        Lwt_unix.sleep 1.0 >>= fun () ->
        Meta.Client.list_games co >>= function
          | Some games -> update games; poll ()
          | None -> assert false (* TODO: close/quit *)
      in
      ignore (create_view ~model ~packing:window#add ());
      Lwt.ignore_result (poll ());

      (* Quit when the window is closed. *)
      ignore (window#connect#destroy (Lwt.wakeup wakener));

      (* Show the window. *)
      window#show ();

      (* Wait for it to be closed. *)
      waiter
    )
