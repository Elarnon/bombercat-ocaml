open Lwt
open Gobject.Data

let cols        = new GTree.column_list
let col_name    = cols#add string
let col_ip      = cols#add string
let col_players = cols#add int

let model = GTree.list_store cols

let update =
  let tbl = Hashtbl.create 17 in
  fun games ->
    let open Protocol.Meta in
    List.iter
      (fun { game_name ; game_addr ; game_nb_players ; game_id ; _ } ->
        let row =
          try
            let (row, seen) = Hashtbl.find tbl game_id in
            seen := true; row
          with Not_found ->
            let row = model # append () in
            Hashtbl.add tbl game_id (row, ref true); row
        in let (ip, port) = Network.raw_addr game_addr in
        model # set ~row ~column:col_name game_name;
        model # set ~row ~column:col_players game_nb_players;
        model # set ~row ~column:col_ip (ip ^ ":" ^ string_of_int port))
      games;
    let to_remove = ref [] in
    Hashtbl.iter (fun id (row, seen) ->
      if !seen then seen := false
      else begin
        to_remove := id :: !to_remove;
        ignore (model # remove row)
      end) tbl;
    List.iter (Hashtbl.remove tbl) !to_remove

let create_view ~model ~packing () =
  let view = GTree.view ~model ~packing () in

  (* First column : name of the game *)
  let col = GTree.view_column ~title:"Name"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_name]) () in
  ignore (view#append_column col);

  (* Second column : IP address of the server *)
  let col = GTree.view_column ~title:"IP address"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_ip]) () in
  ignore (view#append_column col);

  (* Third column : number of places available *)
  let col = GTree.view_column ~title:"Spots availables"
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

  let box = GPack.vbox ~homogeneous:false ~packing:window#add () in

  lwt addr = Network.parse_addr "127.0.0.1:22222" in
  lwt co = Meta.Client.Connection.open_connection addr in
  let rec poll () =
    Lwt_unix.sleep 1.0 >>= fun () ->
    Meta.Client.list_games co >>= function
    | None -> window#destroy (); return ()
    | Some games -> update games; poll ()
  in
  ignore (create_view ~model ~packing:box#pack ());
  Lwt.async poll;

  (* Quit when the window is closed. *)
  ignore (window#connect#destroy (Lwt.wakeup wakener));

  (* Show the window. *)
  window#show ();

  (* Wait for it to be closed. *)
  waiter
)
