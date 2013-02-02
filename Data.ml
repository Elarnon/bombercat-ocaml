open Lwt
  
open CamomileLibrary
  
type map_case =
  | Empty
  | Indestructible
  | Destructible
  | Bomb of int * int * (int * int) Lwt_sequence.node

type dir = | Left | Right | Up | Down

type pos = (int * int)

type map =
  { width : int; height : int;
    content : ((map_case * (char list)) array) array;
    players : (char, (int * int)) Hashtbl.t;
    bombs : (int * int) Lwt_sequence.t
  }

let map_choose { players = players; _ } =
  let module X = struct exception Found of char
                           end
  in
    try (Hashtbl.iter (fun id -> raise (X.Found id)) players; None)
    with | X.Found id -> Some id
  
let map_nb_players map = Hashtbl.length map.players
  
let players { players = players; _ } = players
  
let width { width = width; _ } = width
  
let height { height = height; _ } = height
  
let iter_players f { players = players; _ } = Hashtbl.iter f players
  
let map_get map (x, y) = fst map.content.(x).(y)
  
let map_set map (x, y) v =
  let (_, l) = map.content.(x).(y) in map.content.(x).(y) <- (v, l)
  
let map_players map (x, y) = snd map.content.(x).(y)
  
let map_add_player map (x, y) player =
  let (v, l) = map.content.(x).(y)
  in map.content.(x).(y) <- (v, (player :: l))
  
let map_rm_player map (x, y) player =
  let (v, l) = map.content.(x).(y)
  in map.content.(x).(y) <- (v, (List.filter (fun c -> c <> player) l))
  
let map_clean_players map (x, y) =
  let (v, _) = map.content.(x).(y) in map.content.(x).(y) <- (v, [])
  
let ( ++> ) (x, y) =
  function
  | Left -> ((x - 1), y)
  | Right -> ((x + 1), y)
  | Up -> (x, (y - 1))
  | Down -> (x, (y + 1))
  
let pos map player =
  try Some (Hashtbl.find map.players player) with | Not_found -> None
  
let map_pos m p = Hashtbl.find m.players p
  
let is_pos_valid { width = width; height = height; _ } (x, y) =
  (x >= 0) && ((y >= 0) && ((x < width) && (y < height)))
  
let check_pos map player ppos = (pos map player) = (Some ppos)
  
let after_move map player (x, y) dir =
  if check_pos map player (x, y)
  then
    (match dir with
     | Left -> if x > 0 then Some ((x - 1), y) else None
     | Right -> if x < (map.width - 1) then Some ((x + 1), y) else None
     | Up -> if y > 0 then Some (x, (y - 1)) else None
     | Down -> if y < (map.height - 1) then Some (x, (y + 1)) else None)
  else None
  
let content_in lst map pos =
  try List.mem (map_get map pos) lst with | Invalid_argument _ -> false
  
let is_free_for_move map pos = content_in [ Empty ] map pos
  
let is_free_for_bomb map pos = content_in [ Empty ] map pos
  
let try_move map player pos dir =
  match after_move map player pos dir with
  | None -> false
  | Some npos ->
      if is_free_for_move map npos
      then
        (map_rm_player map pos player;
         Hashtbl.replace map.players player npos;
         map_add_player map npos player;
         true)
      else false
  
let can_bomb map player (((x, y) as pos)) =
  (check_pos map player pos) && (is_free_for_bomb map pos)
  
let try_bomb map player (((x, y) as pos)) timer dist =
  if can_bomb map player pos
  then
    (let node = Lwt_sequence.add_r pos map.bombs
     in (map_set map pos (Bomb (timer, dist, node)); true))
  else false
  
let kill map player =
  match pos map player with
  | Some p ->
      (map_rm_player map p player; Hashtbl.remove map.players player; true)
  | None -> assert false
  
(* wut *)
let blast ?(killed = []) map pos dist =
  let killall map pos k =
    let players = map_players map pos in
    let killed =
      List.fold_left
        (fun killed id -> if kill map id then id :: killed else killed) k
        players
    in killed in
  let rec blast_impl map pos dist killed =
    if is_pos_valid map pos
    then
      (let killed0 = killall map pos killed in
       let killed1 = explode map pos dist Left killed0 in
       let killed2 = explode map pos dist Right killed1 in
       let killed3 = explode map pos dist Up killed2
       in explode map pos dist Down killed3)
    else killed
  and explode map pos dist dir killed =
    if (dist > 0) && (is_pos_valid map pos)
    then
      (match map_get map pos with
       | Empty ->
           let killed' = killall map (pos ++> dir) killed
           in explode map (pos ++> dir) (dist - 1) dir killed'
       | Indestructible -> killed
       | Destructible -> (map_set map pos Empty; killed)
       | Bomb (_, dist', node) ->
           (Lwt_sequence.remove node;
            map_set map pos Empty;
            let killed' = blast_impl map pos dist' killed
            in explode map (pos ++> dir) (dist - 1) dir killed'))
    else killed
  in blast_impl map pos dist killed
  
let decrease_timers (({ bombs = bombs; _ } as map)) =
  Lwt_sequence.fold_l
    (fun pos killed ->
       match map_get map pos with
       | Bomb (timer, dist, node) when timer <= 0 ->
           ((* BOOM! *)
            Lwt_sequence.remove node;
            map_set map pos Empty;
            blast ~killed map pos dist)
       | Bomb (timer, dist, node) ->
           (map_set map pos (Bomb ((timer - 1), dist, node)); killed)
       | _ -> assert false)
    (* Wow, error there. *) bombs []
  
exception InvalidMap
  
let empty_map ~width: (m) ~height: (m') =
  {
    width = m;
    height = m';
    content = Array.init m (fun _ -> Array.make m' (Empty, []));
    players = Hashtbl.create 17;
    bombs = Lwt_sequence.create ();
  }
  
let map_of_string s =
  let l = String.length s in
  let (x, y) = ((ref 0), (ref 0)) in
  let (w, h) = ((ref 0), (ref 0)) in
  let nb_players = ref 0 in
  let (res, players) = ((ref []), (ref []))
  in
    (for i = 0 to l - 1 do
       (match s.[i] with
        | '#' -> res := ((!x), (!y), Indestructible) :: !res
        | '$' -> res := ((!x), (!y), Destructible) :: !res
        | s when ('A' <= s) && (s <= 'Z') ->
            (players := ((!x), (!y), s) :: !players;
             nb_players := !nb_players + 1)
        | '\n' -> (w := max !w !x; x := (-1); y := !y + 1)
        | _ -> ());
       x := !x + 1
     done;
     if s.[l - 1] <> '\n' then (w := !x; y := !y + 1) else ();
     h := !y;
     let map = empty_map ~width: !w ~height: !h
     in
       (List.iter (fun (x, y, v) -> map.content.(x).(y) <- (v, [])) !res;
        List.iter
          (fun (x, y, id) ->
             if Hashtbl.mem map.players id
             then raise InvalidMap
             else
               (Hashtbl.add map.players id (x, y);
                map_add_player map (x, y) id))
          !players;
        map))
  
let string_of_map {
                    width = width;
                    height = height;
                    content = content;
                    players = players
                  } =
  let size = (width + 1) * height in
  let s = String.make size ' ' in
  let put x y v = s.[(y * (width + 1)) + x] <- v
  in
    (Array.iteri
       (fun x ->
          Array.iteri
            (fun y ->
               function
               | (Indestructible, _) -> put x y '#'
               | (Destructible, _) -> put x y '$'
               | (Bomb (_, _, _), _) | (Empty, _) -> ()))
       content;
     Hashtbl.iter (fun c (x, y) -> put x y c) players;
     for i = 0 to height - 1 do put width i '\n' done;
     s)
  
let draw { content = content; players = players; _ } ui matrix =
  let module Ui = LTerm_ui
  in let module Draw = LTerm_draw
    in let module Style = LTerm_style
      in
        let size = Ui.size ui in
        let ctx = Draw.context matrix size
        in
          (Draw.clear ctx;
           Array.iteri
             (fun x ->
                Array.iteri
                  (fun y ->
                     function
                     | (Indestructible, _) ->
                         Draw.draw_char ctx y x (UChar.of_char '#')
                     | (Destructible, _) ->
                         Draw.draw_char ctx y x (UChar.of_char '$')
                     | (Bomb (timer, _, _), _) ->
                         let chr =
                           if timer < 10
                           then char_of_int (timer + (int_of_char '0'))
                           else
                             char_of_int ((timer - 10) + (int_of_char 'A')) in
                         let style = let open Style
                           in
                             {
                               (none)
                               with
                               bold = Some true;
                               background = Some red;
                               foreground = Some yellow;
                             }
                         in Draw.draw_char ctx y x ~style (UChar.of_char chr)
                     | (Empty, _) -> ()))
             content;
           Hashtbl.iter
             (fun c (x, y) -> Draw.draw_char ctx y x (UChar.of_char c))
             players)
  
let display map callback update = let open LTerm_key
  in let open LTerm_event
    in
      let rec loop ui =
        (LTerm_ui.wait ui) >>=
          (function
           | Key { code = Escape } -> return ()
           | ev -> (callback ev; LTerm_ui.draw ui; loop ui)) in
      let rec redraw ui =
        (Lwt_condition.wait update) >>=
          (fun () -> (LTerm_ui.draw ui; redraw ui)) in
      let __pa_lwt_0 = Lazy.force LTerm.stdout
      in
        Lwt.bind __pa_lwt_0
          (fun term ->
             let __pa_lwt_0 = LTerm_ui.create term (draw map)
             in
               Lwt.bind __pa_lwt_0
                 (fun ui ->
                    Lwt.finalize
                      (fun () -> (Lwt.async (fun () -> redraw ui); loop ui))
                      (fun () -> (LTerm_ui.quit ui))))
  

