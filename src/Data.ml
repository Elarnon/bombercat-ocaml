type case =
  | Empty
  | Destructible
  | Indestructible
  | Bomb of int

(* Internally used in place of case to allow for internal treatment on
 * explosions – it helps ensure that, when multiple explosions occur
 * simultaneously, at no explosion "pass through" destructible cases. *)
type variant =
  [ `Empty
  | `Destroying
  | `Indestructible
  | `Destructible
  | `Bomb of int * int * (int * int) Lwt_sequence.node ]

exception InvalidMap

exception Inconsistency of string

(* Conversion from internal variants to exposed cases. *)
let of_variant = function
  | `Empty -> Empty
  | `Destroying ->
      raise (Inconsistency "`Destroying in of_variant (outside of blast).")
  | `Indestructible -> Indestructible
  | `Destructible -> Destructible
  | `Bomb (timer, _, _) -> Bomb timer

type dir = Left | Right | Up | Down

type pos = int * int

type map = 
  { width : int
  ; height : int
  ; content : (variant * char list) array array
  (* On each case, there is the internal variant representing its type, as well
   * as a list of players on this case. *)
  ; players : (char, (int * int)) Hashtbl.t
  ; bombs : (int * int) Lwt_sequence.t
  (* This allows to easily walk through all bombs without searching through the
   * whole map. *)
  }

let map_choose { players; _ } =
  let module X = struct exception Found of char end in
  try Hashtbl.iter (fun id -> raise (X.Found id)) players; None
  with X.Found id -> Some id

let map_nb_players map = Hashtbl.length map.players

let width { width; _ } = width

let height { height; _ } = height

let iter_content f { content; _ } =
  Array.iteri (fun x ->
      Array.iteri (fun y v ->
        f (x, y) (of_variant (fst v))))
    content

let iter_players f { players; _ } =
  Hashtbl.iter f players

let map_get map (x, y) =
  fst map.content.(x).(y)

let map_set map (x, y) v =
  let _, l = map.content.(x).(y) in
  map.content.(x).(y) <- v, l

let map_players map (x, y) =
  snd map.content.(x).(y)

(* Internal add player on the map. Does *only* affect the content. *)
let map_add_player map (x, y) player =
  let v, l = map.content.(x).(y) in
  map.content.(x).(y) <- v, player :: l

(* Internal remove player. Does *only* affect the content. *)
let map_rm_player map (x, y) player =
  let v, l = map.content.(x).(y) in
  map.content.(x).(y) <- v, List.filter (fun c -> c <> player) l

(* Adds a direction to a position *)
let (++>) (x, y) = function
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)

let pos map player =
  try Some (Hashtbl.find map.players player)
  with Not_found -> None

let map_pos map player = Hashtbl.find map.players player

let is_pos_valid { width; height; _ } (x, y) =
  x >= 0 && y >= 0 && x < width && y < height

let check_pos map player ppos =
  pos map player = Some ppos

(* Computes the position of player after a move from (x, y) in direction dir.
 * Incorporates the check that player's position is indeed (x, y), but does'nt
 * chec that the end position is empty nor anything. *)
let after_move map player (x, y) dir =
  if check_pos map player (x, y) then
    match dir with
    | Left ->
      if x > 0 then Some (x - 1, y) else None
    | Right -> 
      if x < map.width - 1 then Some (x + 1, y) else None
    | Up ->
      if y > 0 then Some (x, y - 1) else None
    | Down ->
      if y < map.height - 1 then Some (x, y + 1) else None
  else None

let content_in lst map pos =
  try List.mem (map_get map pos) lst with Invalid_argument _ -> false

(* Players can move on empty positions only *)
let is_free_for_move map pos =
  content_in [ `Empty ] map pos

(* Players can only pose bombs on empty cases — for instance, two bombs can't be
 * on the same place. *)
let is_free_for_bomb map pos =
  content_in [ `Empty ] map pos

let try_move map player pos dir =
  match after_move map player pos dir with
    | None -> false
    | Some npos ->
        if is_free_for_move map npos then begin
          map_rm_player map pos player;
          Hashtbl.replace map.players player npos;
          map_add_player map npos player;
          true
        end else false

(* Checks that bombing at position pos is valid for player *)
let can_bomb map player pos =
  check_pos map player pos && is_free_for_bomb map pos

let try_bomb map player pos timer dist =
  if can_bomb map player pos then begin
    let node = Lwt_sequence.add_r pos map.bombs in
    map_set map pos (`Bomb (timer, dist, node));
    true
  end else false

(* Internally called by blast only. Kills a player *)
let kill map player =
  match pos map player with
  | Some p -> begin
    map_rm_player map p player;
    Hashtbl.remove map.players player;
    true
  end
  | None ->
      raise (Inconsistency "kill called on bad input.")

(* Computes an explosion. TODO: explain the algorithm. *)
let blast ?(killed=[]) map pos dist =
  let killall map pos k =
    let players = map_players map pos in
    let killed = List.fold_left
      (fun killed id -> if kill map id then id :: killed else killed) k players
    in killed
  in let rec blast_impl map pos dist killed destroyed =
    if is_pos_valid map pos then begin
      let killed0 = killall map pos killed in
      let killed1 = explode map pos dist Left killed0 destroyed in
      let killed2 = explode map pos dist Right killed1 destroyed in
      let killed3 = explode map pos dist Up killed2 destroyed in
      explode map pos dist Down killed3 destroyed
    end else killed
  and explode map pos dist dir killed destroyed =
    if dist > 0 && is_pos_valid map pos then
      match map_get map pos with
      | `Empty -> begin
        let killed' = killall map (pos ++> dir) killed in
        explode map (pos ++> dir) (dist - 1) dir killed' destroyed
      end
      | `Indestructible -> killed
      | `Destructible ->
          destroyed := pos :: !destroyed;
          map_set map pos `Destroying; killed
      | `Destroying -> killed
      | `Bomb (_, dist', node) -> begin
        Lwt_sequence.remove node;
        map_set map pos `Empty;
        let killed' = blast_impl map pos dist' killed destroyed in
        explode map (pos ++> dir) (dist - 1) dir killed' destroyed
      end
    else killed
  in
    let destroyed = ref [] in
    let res = blast_impl map pos dist killed destroyed in
    List.iter (fun pos -> map_set map pos `Empty) !destroyed;
    res

let decrease_timers ({ bombs; _ } as map) =
  Lwt_sequence.fold_l
    (fun pos killed ->
      match map_get map pos with
      | `Bomb (timer, dist, node) when timer <= 0 -> begin (* BOOM! *)
        Lwt_sequence.remove node;
        map_set map pos `Empty;
        blast ~killed map pos dist
      end
      | `Bomb (timer, dist, node) -> begin
        map_set map pos (`Bomb (timer - 1, dist, node));
        killed
      end
      | _ ->
          raise (Inconsistency "Bomb nodes badly synchronized."))
    bombs
    []

let empty_map ~width:m ~height:m' =
  { width = m
  ; height = m'
  ; content = Array.init m (fun _ -> Array.make m' (`Empty, []))
  ; players = Hashtbl.create 17
  ; bombs = Lwt_sequence.create ()
  }

let map_of_string s =
  let l = String.length s in
  let x, y = ref 0, ref 0 in
  let w, h = ref 0, ref 0 in
  let nb_players = ref 0 in
  let res, players = ref [], ref [] in
  for i = 0 to l - 1 do
    begin match s.[i] with
    | '#' -> res := (!x, !y, `Indestructible) :: !res
    | '$' -> res := (!x, !y, `Destructible) :: !res
    | s when 'A' <= s && s <= 'Z' ->
        players := (!x, !y, s) :: !players;
        nb_players := !nb_players + 1
    | '\n' -> w := max !w !x; x := -1; y := !y + 1
    | ' ' -> ()
    | _ ->
        raise InvalidMap
    end;
    x := !x + 1;
  done;
  if s.[l - 1] <> '\n' then begin
    w := !x;
    y := !y + 1
  end;
  h := !y;
  let map = empty_map ~width:!w ~height:!h in
  List.iter (fun (x, y, v) -> map.content.(x).(y) <- v, []) !res;
  List.iter (fun (x, y, id) ->
    if Hashtbl.mem map.players id
    then raise InvalidMap
    else begin
      Hashtbl.add map.players id (x, y); map_add_player map (x, y) id
    end) !players;
  map

let string_of_map { width; height; content; players; _ } =
  let size = (width + 1) * height in
  let s = String.make size ' ' in
  let put x y v = s.[y * (width + 1) + x] <- v in
  Array.iteri
    (fun x ->
      Array.iteri
        (fun y ->
          function
            | `Indestructible, _ -> put x y '#'
            | `Destructible, _ -> put x y '$'
            | `Destroying, _ ->
                raise (Inconsistency ("`Destroying in string_of_map (outside "
                ^ "of blast)."))
            | `Bomb (_, _, _), _ | `Empty, _ -> ()))
    content;
  Hashtbl.iter
    (fun c (x, y) -> put x y c)
    players;
  for i = 0 to height - 1 do
    put width i '\n'
  done;
  s
