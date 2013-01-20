type map_case =
  | Empty
  | Indestructible
  | Destructible
  | Bomb of int

type dir = Left | Right | Up | Down

type map = 
  { width : int
  ; height : int
  ; content : map_case array array
  ; players : (char, (int * int)) Hashtbl.t
  }

let pos map player =
  try
    Some (Hashtbl.find map.players player)
  with Not_found -> None

let check_pos map player ppos =
  pos map player = Some ppos

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

let content_in lst map (x, y) =
  try List.mem map.content.(x).(y) lst with Invalid_argument _ -> false

let is_free_for_move map pos =
  content_in [ Empty ] map pos

let is_free_for_bomb map pos =
  content_in [ Empty ] map pos

let try_move map player pos dir =
  match after_move map player pos dir with
    | None -> false
    | Some pos ->
      if is_free_for_move map pos then begin
        Hashtbl.replace map.players player pos; true
      end else false

let try_bomb map player ((x, y) as pos) timer =
  if check_pos map player pos && is_free_for_bomb map pos then begin
    map.content.(x).(y) <- Bomb timer; true
  end else false

exception InvalidMap

let empty_map ~width:m ~height:m' =
  { width = m
  ; height = m'
  ; content = Array.init m (fun _ -> Array.make m' Empty)
  ; players = Hashtbl.create 17
  }

let map_of_string s =
  let l = String.length s in
  let x, y = ref 0, ref 0 in
  let w, h = ref 0, ref 0 in
  let nb_players = ref 0 in
  let res, players = ref [], ref [] in
  for i = 0 to l - 1 do
    begin match s.[i] with
    | '#' -> res := (!x, !y, Indestructible) :: !res
    | '$' -> res := (!x, !y, Destructible) :: !res
    | s when 'A' <= s && s <= 'Z' ->
        players := (!x, !y, s) :: !players;
        nb_players := !nb_players + 1
    | '\n' -> w := max !w !x; x := -1; y := !y + 1
    | _ -> ()
    end;
    x := !x + 1;
  done;
  if s.[l - 1] <> '\n' then begin
    w := !x;
    y := !y + 1
  end;
  h := !y;
  let map = empty_map ~width:!w ~height:!h in
  List.iter (fun (x, y, v) -> map.content.(x).(y) <- v) !res;
  List.iter (fun (x, y, id) ->
    if Hashtbl.mem map.players id
    then raise InvalidMap
    else Hashtbl.add map.players id (x, y)) !players;
  map

let string_of_map { width; height; content; players } =
  let size = (width + 1) * height in
  let s = String.make size ' ' in
  let put x y v = s.[y * (width + 1) + x] <- v in
  Array.iteri
    (fun x ->
      Array.iteri
        (fun y ->
          function
            | Indestructible -> put x y '#'
            | Destructible -> put x y '$'
            | Bomb _ | Empty -> ()))
    content;
  Hashtbl.iter
    (fun c (x, y) -> put x y c)
    players;
  for i = 0 to height - 1 do
    put width i '\n'
  done;
  s
