open Misc
open Lwt
open Initialisation
open Protocol
open Protocol.Initialisation
open Protocol.Game
open CamomileLibrary

type t =
  { map : Data.map
  ; map_id : char
  ; ui : LTerm_ui.t
  ; current : int ref
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


let create g_map { p_game_time; _ } me =
  lwt term = Lazy.force LTerm.stdout in
  let current = ref 0 in
  lwt ui = LTerm_ui.create term (draw g_map p_game_time me current) in
  LTerm_ui.set_cursor_visible ui true;
  return { map = g_map
  ; map_id = me
  ; ui
  ; current
  }

let update { ui; current; _ } turn =
  current := turn;
  LTerm_ui.draw ui

let rec input ({ map; map_id; ui; _ } as t) =
  let open LTerm_key in
  let open LTerm_event in
  LTerm_ui.wait ui >>= fun evt ->
    match evt with
    | Key { code = Escape } ->
        return None
    | Key { code } -> begin try
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

let quit { ui; _ } =
  LTerm_ui.quit ui
