open Misc
open Lwt
open Protocol.Game
open Protocol.Meta
open CamomileLibrary
open LTerm_geom

let data = ref None

module Meta = struct
  type t =
    { m_ui : LTerm_ui.t
    ; m_games : Protocol.Meta.game list ref
    ; m_looping : Protocol.Meta.game option Lwt.t
    ; m_errors : string Queue.t
    ; mutable m_freed : bool
    }

  let id_before lst key =
    List.fold_left
      (fun acc { game_id; _ } ->
        if key = None || Some game_id < key then
          Some game_id
        else acc)
      None
      lst

  let id_after lst key =
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
      | Key { code = Up; _ } ->
          begin match id_before !lst !current with
          | None -> ()
          | Some k -> current := Some k end;
          LTerm_ui.draw ui;
          loop ui current lst
      | Key { code = Down; _ } ->
          begin match id_after !lst !current with
          | None -> ()
          | Some k -> current := Some k end;
          LTerm_ui.draw ui;
          loop ui current lst
      | Key { code = Enter; _ } ->
          begin match !current with
          | None -> loop ui current lst
          | Some id ->
              begin try
                return @$
                  Some (List.find (fun { game_id; _ } -> game_id = id) !lst)
              with Not_found -> loop ui current lst end
          end
      | Key { code = Escape; _ } ->
          return None
      | _ev -> loop ui current lst

  let draw error errors current games ui matrix =
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
      games;
    if !error = None && not (Queue.is_empty errors) then begin
      let err = Queue.take errors in
      error := Some err
    end;
    match !error with
    | None -> ()
    | Some err ->
        let (row, col) = string_size err in
        let rstart = (size.rows - 1 - row) / 2
        and rend = (size.rows - 1 + row) / 2
        and cstart = (size.cols + 1 - col) / 2
        and cend = (size.cols + 1 + col) / 2 in
        let sub = D.sub ctx { row1 = rstart - 2; row2 = rend + 3
                            ; col1 = cstart - 1; col2 = cend + 1} in
        D.clear sub;
        D.draw_frame
          sub { row1 = 0; row2 = row + 5; col1 = 0; col2 = col + 2 }
          D.Heavy;
        D.draw_string_aligned sub 1 H_align_center err;
        D.draw_hline sub (row + 2) 1 col D.Light;
        let style = LTerm_style.({ none with reverse = Some true }) in
        let ok_ctx =
          D.sub sub
                { row1 = row + 3; row2 = row + 4; col1 = 1; col2 = col / 2 }
        in D.draw_string_aligned ~style ok_ctx 0 H_align_center "OK";
        let quit_ctx =
          D.sub sub
                { row1 = row + 3; row2 = row + 4; col1 = col / 2; col2 = col }
        in D.draw_string_aligned quit_ctx 0 H_align_center "QUIT"

  let init () =
    if !data <> None then
      raise Display.Device_in_use
    else
    lwt term = Lazy.force LTerm.stdout in
    let current = ref None in
    let games = ref [] in
    let errors = Queue.create () in
    let e = ref None in
    lwt ui =
      LTerm_ui.create term (fun ui m -> draw e errors !current !games ui m) in
    data := Some ui;
    return
      { m_games = games
      ; m_looping = loop ui current games
      ; m_ui = ui
      ; m_errors = errors
      ; m_freed = false }

  let check = function
    | true -> fail Display.Invalid_resource
    | false -> return_unit

  let update { m_games; m_ui; m_freed; _ } games =
    if m_freed then raise Display.Invalid_resource;
    m_games := 
      List.sort
        (fun { game_id; _ } { game_id = id; _ } -> compare game_id id)
        games;
    LTerm_ui.draw m_ui

  let error { m_freed; m_errors; m_ui; _ } error =
    if m_freed then raise Display.Invalid_resource;
    Queue.add error m_errors;
    LTerm_ui.draw m_ui

  let input { m_looping; m_freed; _ } =
    check m_freed >>
    m_looping

  let free ({ m_ui; _ } as t) =
    check t.m_freed >>= fun () ->
    data := None;
    t.m_freed <- true;
    LTerm_ui.quit m_ui
end

module Init = struct
  type t = LTerm_ui.t * bool ref

  let draw ui matrix =
    let module Ui = LTerm_ui in
    let module Draw = LTerm_draw in
    let module Style = LTerm_style in
    let size = Ui.size ui in
    let ctx = Draw.context matrix size in
    Draw.clear ctx;
    let middle = size.rows / 2 in
    Draw.draw_string_aligned ctx middle H_align_center
      "Initializing game… Please wait."

  let init _game =
    if !data <> None then fail Display.Device_in_use else begin
      lwt term = Lazy.force LTerm.stdout in
      lwt ui = LTerm_ui.create term draw in
      data := Some ui;
      LTerm_ui.draw ui;
      return (ui, ref false)
    end

  let rec input (ui, freed) =
    if !freed then fail Display.Invalid_resource else
    let open LTerm_event in
    let open LTerm_key in
    match_lwt LTerm_ui.wait ui with
    | Key { code = Escape; _ } ->
        return ()
    | _ev ->
        input (ui, freed)

  let free (ui, freed) =
    if !freed then fail Display.Invalid_resource else begin
      data := None;
      freed := true;
      LTerm_ui.quit ui
    end
end

module Game = struct

  type t =
    { map : Data.map
    ; map_id : char
    ; ui : LTerm_ui.t
    ; current : int ref
    ; freed : bool ref
    }

  let draw map time id turn ui matrix =
    let module Ui = LTerm_ui in
    let module Draw = LTerm_draw in
    let module Style = LTerm_style in
    let open Data in
    let size = Ui.size ui in
    let ctx = Draw.context matrix size in
    Draw.clear ctx;
    Data.iter_content (fun (x, y) -> function
      | Indestructible ->
          Draw.draw_char ctx (1+y) x (UChar.of_char '#')
      | Destructible ->
          Draw.draw_char ctx (1+y) x (UChar.of_char '$')
      | Bomb timer ->
          let chr =
            if timer < 10 then
              char_of_int (timer + int_of_char '0')
            else char_of_int (timer - 10 + int_of_char 'a') in
          let style =
            Style.({ none with bold = Some true ; background = Some red 
            ; foreground = Some yellow}) in
          Draw.draw_char ctx (1+y) x ~style (UChar.of_char chr)
      | Empty -> ()) map;
    let present = ref false in
    Data.iter_players (fun c (x, y) ->
      if c = id then begin
        present := true;
        LTerm_ui.set_cursor_visible ui true;
        LTerm_ui.set_cursor_position ui
          LTerm_geom.({ row = 1 + y; col = x });
      end;
      Draw.draw_char ctx (1+y) x (UChar.of_char c)) map;
    if not !present then
      LTerm_ui.set_cursor_visible ui false;
    Draw.draw_string ctx 0 0 (Format.sprintf "Tour %4i/%4i" !turn time)


  let init players map time idme =
    if !data <> None then fail Display.Device_in_use else begin
      (* TODO catch Not_found *)
      let me = snd @$ Hashtbl.find players idme in
      lwt term = Lazy.force LTerm.stdout in
      let current = ref 0 in
      lwt ui = LTerm_ui.create term (draw map time me current) in
      data := Some ui;
      LTerm_ui.set_cursor_visible ui true;
      return { map
      ; map_id = me
      ; ui
      ; current
      ; freed = ref false
      }
    end

  let update { freed; ui; current; _ } turn =
    if !freed then raise Display.Invalid_resource;
    current := turn;
    LTerm_ui.draw ui

  let rec input ({ freed; map; map_id; ui; _ } as t) =
    if !freed then fail Display.Invalid_resource else
    let open LTerm_key in
    let open LTerm_event in
    LTerm_ui.wait ui >>= fun evt ->
      match evt with
      | Key { code = Escape; _ } ->
          return None
      | Key { code; _ } -> begin try
        let pos = Data.map_pos map map_id in
        match code with
        | Left ->
            return @$ Some (MOVE (pos, Data.Left))
        | Up ->
            return @$ Some (MOVE (pos, Data.Up))
        | Down ->
            return @$ Some (MOVE (pos, Data.Down))
        | Right ->
            return @$ Some (MOVE (pos, Data.Right))
        | Char c when c = UChar.of_char 'b' ->
            return @$ Some (BOMB pos)
        | _ -> input t
      with Not_found -> input t end
      | _ ->
          input t

  let free { freed; ui; _ } =
    if !freed then fail Display.Invalid_resource else begin
      data := None;
      freed := true;
      LTerm_ui.quit ui
    end

end

let quit () =
  match !data with
  | None -> return_unit
  | Some ui -> LTerm_ui.quit ui
