open Misc
open Data

exception Error of string

type client_command =
  | MOVE of pos * dir
  | BOMB of pos

type client =
  | COMMAND of string * int * int * client_command
  | SYNC of string * int

type action =
  | CLIENT of client_command
  | NOP of int
  | DEAD

type stats =
  { winner : string option }

type server =
  | TURN of int * action list Smap.t (* Imap *)
  | GAMEOVER of int * stats

open Bencode

let encode_dir = function
  | Up -> S "UP"
  | Right -> S "RIGHT"
  | Down -> S "DOWN"
  | Left -> S "LEFT"

let decode_dir = function
  | S "UP" -> Up
  | S "RIGHT" -> Right
  | S "DOWN" -> Down
  | S "LEFT" -> Left
  | _ -> raise (Error "Invalid dir")

let encode_pos (x, y) = L [I x; I y]

let decode_pos = function
  | L [I x; I y] -> (x, y)
  | _ -> raise (Error "Invalid position.")

let encode_client_command = function
  | MOVE (pos, dir) -> L [S "MOVE"; encode_pos pos; encode_dir dir]
  | BOMB pos -> L [S "BOMB"; encode_pos pos]

let decode_client_command = function
  | L [S "MOVE"; pos; dir] -> MOVE (decode_pos pos, decode_dir dir)
  | L [S "BOMB"; pos] -> BOMB (decode_pos pos)
  | _ -> raise (Error "Invalid client")

let encode_client = function
  | COMMAND (id, seq, rej, cmd) ->
      D (map_of_list
        [ ("Id", S id)
        ; ("Sequence", I seq)
        ; ("Reject", I rej)
        ; ("Message", encode_client_command cmd)
        ])
  | SYNC (id, i) ->
      D (map_of_list [ ("Id", S id); ("Message", L [S "SYNC"; I i]) ])

let encode_clients lst = L (List.map encode_client lst)

let most_clients_to_string lst =
  most_to_string 65535 (List.map encode_client lst)

let all_clients_to_strings lst =
  all_to_strings 65535 (List.map encode_client lst)

let decode_client = function
  | D tbl -> begin
    try
      match gets [ "Id"; "Message" ] tbl with
      | [S id; L [S "SYNC"; I i]] -> SYNC (id, i)
      | [S id; cmd] ->
          begin match gets ["Sequence"; "Reject"] tbl with
          | [I sq; I rj] -> COMMAND (id, sq, rj, decode_client_command cmd)
          | _ -> raise (Error "bad")
          end
      | _ -> raise (Error "worse")
    with Not_found -> raise (Error "Not_found")
  end
  | _ -> raise (Error "bad client")

let decode_clients = function
  | L lst -> List.map decode_client lst
  | b -> [decode_client b]

let encode_action = function
  | CLIENT client -> encode_client_command client
  | NOP i -> L [S "NOP"; I i]
  | DEAD -> S "DEAD"

let decode_action = function
  | L [S "NOP"; I i] -> NOP i
  | S "DEAD" -> DEAD
  | cli -> CLIENT (decode_client_command cli)

let encode_stats { winner } =
  match winner with
  | Some w -> D (Smap.add "WINNER" (S w) Smap.empty)
  | None -> D Smap.empty

let decode_stats = function
  | D tbl -> begin
    try begin match Smap.find "WINNER" tbl with
    | S winner -> { winner = Some winner }
    | _ -> raise (Error "Winner not an identifier.")
    end with Not_found -> { winner = None }
  end
  | _ -> raise (Error "bad stats")

let encode_server ?nops = function
  | TURN (i, tbl) ->
      let btbl = Smap.mapi
          (fun id v ->
            let l =
              if nops = Some id then v else
                List.filter (function | NOP _ -> false | _ -> true) v in
            L (List.map encode_action l)
          ) tbl in
      L [S "TURN"; I i; D btbl]
  | GAMEOVER (i, stats) -> L [S "GAMEOVER"; I i; encode_stats stats]

let encode_servers ?nops lst =
  L (List.map (encode_server ?nops) lst)

let most_servers_to_string ?nops lst =
  most_to_string 65535 (List.map (encode_server ?nops) lst)

let all_servers_to_strings ?nops lst =
  all_to_strings 65535 (List.map (encode_server ?nops) lst)

let decode_server = function
  | L [S "TURN"; I i; D btbl] ->
      TURN (i, Smap.map (function
        | L lst -> List.map decode_action lst
        | _ -> raise (Error "bad actions")) btbl)
  | L [S "GAMEOVER"; I i; stats] -> GAMEOVER (i, decode_stats stats)
  | _ -> raise (Error "bad server")

let decode_servers = function
  | L lst ->
      begin try [decode_server @$ L lst]
      with Error _ -> List.map decode_server lst end
  | b -> [decode_server b] (* always fail... *)

let string_to_servers s =
  try
    match Bencode.of_string s with
      | None -> []
      | Some b -> decode_servers b
  with
  | Bencode.Format_error _ -> []
  | Error _ -> []

let string_to_clients s =
  try match Bencode.of_string s with
      | None -> []
      | Some b -> decode_clients b
  with
  | Bencode.Format_error _ -> []
  | Error _ -> []
