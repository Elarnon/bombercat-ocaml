open Lwt
open Gobject.Data

let start : (int -> unit) ref = ref (fun _ -> ())

let mk_row id =
  let open LTerm_widget in
  let row = new hbox in
  let rname = new button "" in
  let raddr = new label "" in
  let rplayers = new label "" in
  rname#on_click (fun () -> !start id);
  row#add rname; row#add raddr; row#add rplayers;
  (row, rname, raddr, rplayers)

let set_row (_, rn, ra, rp) name addr players =
  rn#set_label name; ra#set_text addr; rp#set_text players

let model = new LTerm_widget.vbox

let update =
  let tbl = Hashtbl.create 17 in
  fun games ->
    let open Protocol.Meta in
    List.iter
      (fun { game_name; game_addr; game_nb_players; game_max_players; game_id
        ; _ } ->
          let row =
            try
              let (row, seen) = Hashtbl.find tbl game_id in
              seen := true; row
            with Not_found ->
              let row = mk_row game_id in
              let r, _, _, _ = row in
              model#add r;
              Hashtbl.add tbl game_id (row, ref true); row
          in let (ip, port) = Network.raw_addr game_addr in
          let players =
            string_of_int game_nb_players ^ "/" ^ string_of_int game_max_players
          in
          set_row row game_name (ip ^ ":" ^ string_of_int port) players)
      games;
    let to_remove = ref [] in
    Hashtbl.iter (fun id (row, seen) ->
      if !seen then seen := false
      else begin
        to_remove := id :: !to_remove;
        let r, _, _, _ = row in
        ignore (model#remove r)
      end) tbl;
    List.iter (Hashtbl.remove tbl) !to_remove

let init () =
  let open LTerm_widget in
  let row = new hbox in
  row#add (new label "Name");
  row#add (new label "IP address");
  row#add (new label "Players");
  model#add row;
  model#add (new hline)

let () = Lwt_main.run (
  let waiter, wakener = wait () in
  lwt term = Lazy.force LTerm.stdout in
  init ();
  let frame = new LTerm_widget.frame in
  frame#set model;
  frame#on_event (function | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape
  } -> wakeup wakener (); true| _ -> false);
  model#set_allocation (LTerm_geom.({ row1 = 0
                                    ; col1 = 0
                                    ; row2 = 10
                                    ; col2 = 40 }));

  lwt addr = Network.parse_addr "127.0.0.1:22222" in
  lwt co = Meta.Client.Connection.open_connection addr in
  start := (fun id -> assert false);
  let rec poll () =
    Lwt_unix.sleep 1.0 >>= fun () ->
    Meta.Client.list_games co >>= function
    | None -> wakeup wakener (); return ()
    | Some games -> update games; model#queue_draw; poll ()
  in
  Lwt.async poll;
  LTerm_widget.run term frame waiter
)
