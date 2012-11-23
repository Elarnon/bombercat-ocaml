type command =
  | Move of pos * dir
  | Bomb of pos

module type INPUT = sig
  val next_command : unit -> command option
  val disconnect : unit -> unit
end

module type OUTPUT = sig
  val display_turn : turn -> unit
end

type obj =
  | Empty
  | Indestructible
  | Destructible
  | Bomb of int
  
type pos = int * int
  
type player_pos =
  | Pos of pos
  | Dead

type player =
  { pos : player_pos
  ; id : int
  ; input : (module INPUT) option
  }

type world =
  { map : obj array array
  ; players : player array
  ; bombs : pos list
  }
  
let game_step { map; players } =
  let next_command p = match p.input with
  | None -> None
  | Some i ->
      let module Input = (val i : INPUT) in
      Input.next_command ()
  and process_command c p =
    match c, p.pos with
    | _, Dead -> None
    | Move (pos, _), pos' | Bomb pos, pos' when pos <> pos' -> None
    | Move (pos, dir), _ ->
        players.(p.id) <- { player with pos = Pos (pos <+> dir) }
    | Bomb (x, y), _ ->
        map.(x).(y) <- Bomb 5
  in let rec process_player p =
    match next_command p with
    | None -> None
    | Some c -> begin match process_command c p with
      | None -> process_player p
      | Some c -> Some c end
  let process_player p =
    let commands = get_commands p in
    List.fold_left
      (fun turn command 
  let inputs = Array.fold_left
    (fun l p ->
      match p.input with
      | None -> l
      | Some i ->
          let module Input = (val i : INPUT) in
          List.map (fun c -> (p, c)) (Input.get_commands ()) :: l)
    [] players in

type turn =
  { bombs : ( player * pos ) list
  ; moves : ( player * pos * dir ) list
  ; explosions : ( player * pos list * player list * pos ) list
  }

let mk_world ~nb_players:n ~width:m ~height:m' =
  { map = Array.init m (fun _ -> Array.make m' Empty)
  ; players = Array.make n { pos = Dead }
  }
  
let bencode_world { map; players } = 
  let open Bencode in
  let l_map =
    Array.fold_right
      (fun line l_map ->
        Array.fold_right
          (fun obj l_map -> match obj with
            | Empty -> String "E" :: l_map
            | Indestructible -> String "I" :: l_map
            | Destructible -> String "D" :: l_map
            | Bomb t -> List [String "B"; Integer t] :: l_map)
          line
          l_map)
      map [] in
  let l_players =
    Array.fold_right
      (fun player l_players -> match player.pos with
            | Dead -> String "D" :: l_players
            | Pos (x, y) -> List [Integer x; Integer y] :: l_players)
      players [] in
  List [ Integer (Array.length players); Integer (Array.length map)
       ; Integer (Array.length map.(0)); List l_map; List l_players ]
  
let read_bencoded_world =
  let open Bencode in function
    | List
      [ Integer nb_players; Integer width; Integer height; List map
      ; List players ] ->
        let world = mk_world ~nb_players ~width ~height in
        ignore (List.fold_left
          (fun (x, y) v ->
            if x >= width then failwith "read_world" else
            begin match v with
            | String "E" -> world.map.(x).(y) <- Empty
            | String "I" -> world.map.(x).(y) <- Indestructible
            | String "D" -> world.map.(x).(y) <- Destructible
            | List [String "B"; Integer t] -> world.map.(x).(y) <- Bomb t
            | _ -> failwith "read_world"
            end;
            if y < height - 1 then (x, y + 1)
            else (x + 1, 0))
        (0, 0) map);
        ignore (List.fold_left
          (fun i v ->
            if i >= nb_players then failwith "read_world" else
            begin match v with
            | List [Integer x; Integer y] ->
                world.players.(i) <- { world.players.(i) with pos = Pos (x, y) }
            | String "D" ->
                world.players.(i) <- { world.players.(i) with pos = Dead }
            | _ -> failwith "read_world"
            end;
            i + 1)
        0 players);
        world
    | v -> failwith "read_world"
    
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
      | l when 'A' <= l && l <= 'Z' ->
          players := (!x, !y, int_of_char l - int_of_char 'A') :: !players;
          nb_players := !nb_players + 1
      | '\n' -> w := !x; x := -1; y := !y + 1
      | _ -> ()
    end;
    x := !x + 1
  done;
  h := !y;
  (* print_int !w; print_endline ""; print_int !h; print_newline (); *)
  let world = mk_world ~nb_players:!nb_players ~width:!w ~height:!h in
  List.iter (fun (x, y, v) -> world.map.(x).(y) <- v) !res;
  List.iter
    (fun (x, y, id) ->
      world.players.(id) <- { world.players.(id) with pos = Pos (x, y) })
    !players;
  world

let display_world { map; players } =
  let display =
    Array.init (Array.length map) (fun _ -> Array.make (Array.length map.(0)) "")
  in
  let put x y v = display.(x).(y) <- v in
  Array.iteri
    (fun x ->
      Array.iteri
        (fun y ->
          function
            | Empty -> put x y " "
            | Indestructible -> put x y "#"
            | Destructible -> put x y "$"
            | Bomb t -> put x y (string_of_int t)))
      map;
  Array.iteri
    (fun i -> function { pos } ->
      match pos with
      | Dead -> ()
      | Pos (x, y) ->
          match display.(x).(y) with
          | " " ->
              display.(x).(y) <- "\x1b[" ^ string_of_int (31 + i) ^ "m@\x1b[0m"
          | v ->
              display.(x).(y) <- "\x1b[" ^ string_of_int (41 + i) ^ "m" ^ v ^
              "\x1b[0m")
    players;
  print_string ( "\x1b[0G\x1b[" ^ string_of_int (Array.length map) ^ "F");
  Array.iter
    (fun a ->
      Array.iter
        print_string
        a;
      print_newline ())
    display

let map_of_string = assert false
let string_of_map = assert false
    
(*
let wstr = let s = ref "" in let f = open_in "world" in begin try while true do s:= !s ^ input_line f ^ "\n" done with End_of_file -> close_in f end; !s ;;

let _ = display_world (read_world wstr) *)
(* 
let w = { map = [| [| Empty; Destructible; Indestructible |] ; [| Bomb 2; Bomb
3; Bomb 4 |] |]; players = [| { pos = Pos (1, 1) }; { pos = Pos (0, 1) } |] } ;;

let _ = display_world w *)
