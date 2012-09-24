type dir =
  | N | E | S | W
  
let dir_of_int = function
  | 0 -> N
  | 1 -> E
  | 2 -> S
  | 3 -> W
  | _ -> failwith "dir_of_int"
  
let int_of_dir = function
  | N -> 0
  | E -> 1
  | S -> 2
  | W -> 3

type client_msg =
  | MOVE of pos * dir
  | BOMB of pos

type server_msg =
  | MOVE_OK of player * pos * dir
  | BOMB_OK of player * pos
  | EXPLOSION of pos list * player list * pos * int
  
let bencode_client_msg =
  let open Bencode in function
    | MOVE ( (x, y), d ) ->
        List [ String "MOVE"; Integer x; Integer y; String (string_of_dir d) 				default:
					break;
]
    | BOMB ( x, y ) ->
        List [ STRING "BOMB"; Integer x; Integer y ]
        
let bencode_server_msg =
  let open Bencode in function
    | MOVE_OK of player * pos * dir ->
        List [
          String "MOVE";
          Integer (player.id);
          Integer (pos.x);
          Integer (pos.y);
          String (string_of_dir d)
        ]
    | BOMB_OK of player * pos ->
        List [
          String "BOMB";
          Integer player.id;
          Integer pos.x;
          Integer pos.y
        ]
    | EXPLOSION (destructed, killed, pos, range) ->
        List [
          String "EXPLOSION";
          List (List.map bencode_pos destructed);
          List (List.map bencode_player_id killed);
          Integer pos.x;
          Integer pos.y;
          Integer range
        ]
