open Lwt
open Network
open Misc
open Bencode
open Data

exception Error of string

let failwith s = fail (Error s)

let read_stream decoder catcher s =
  Lwt_stream.is_empty s >>= function
    | true -> return_none
    | false ->
        try_lwt
          Bencode.of_stream s >>= function
            | Some x -> decoder x >|= fun x -> Some x
            | None -> return_none
        with
          | exn -> catcher exn >> return None

module Meta = struct
  type client =
    | ADD of addr * string * int
    | UPDATE of int * int
    | DELETE of int
    | LIST

  type game =
    { game_id : int
    ; game_addr : Network.addr
    ; game_name : string
    ; game_nb_players : int
    ; game_max_players : int
    }

  type server =
    | ADDED of int
    | GAMES of game list

  let encode_client = function
    | ADD (addr, name, nb_players) ->
        let (ip, port) = raw_addr addr in
        Bencode.(L [
          S "ADD";
          S ip;
          I port;
          S name;
          I nb_players
        ])
    | UPDATE (game_id, nb_players) ->
        Bencode.(L [
          S "UPDATE";
          I game_id;
          I nb_players
        ])
    | DELETE game_id ->
        Bencode.(L [
          S "DELETE";
          I game_id
        ])
    | LIST -> Bencode.S "LIST"

  let decode_client = let open Bencode in function
    | L ( S "ADD" :: S ip :: I port :: S name :: I nb_players :: _ ) ->
        mk_addr ~port ip >>= fun addr -> (* TODO: try/catch *)
        if nb_players < 0
        then
          fail (Error 
            ("Unexpected negative number of players while reading ADD message "
            ^ "from a Protocol.Meta client."))
        else return (ADD (addr, name, nb_players))
    | L ( S "UPDATE" :: I game_id :: I nb_players :: _ ) ->
        if nb_players < 0
        then
          fail (Error
            ("Unexpected negative number of players while reading ADD message "
            ^ "from a Protocol.Meta client."))
        else return (UPDATE (game_id, nb_players))
    | L ( S "DELETE" :: I game_id :: _ ) ->
        return (DELETE game_id)
    | S "LIST" -> return LIST
    | v ->
        fail (Error "Invalid message from Protocol.Meta client.")

  let bencode_game g =
    let open Bencode in
    let (ip, port) = raw_addr g.game_addr in
    D (map_of_list
      [ ("Id", I g.game_id)
      ; ("Ip", S ip)
      ; ("Port", I port)
      ; ("Name", S g.game_name)
      ; ("Players", I g.game_nb_players)
      ; ("MaxPlayers", I g.game_max_players)
      ])

  let bdecode_game = let open Bencode in function
    | D m -> begin
      try
        match gets ["Id"; "Ip"; "Port"; "Name"; "Players"; "MaxPlayers"] m with
        | [I id; S ip; I port; S name; I players; I max_players] when
          players >= 0 && max_players >= players && max_players > 0 ->
            lwt game_addr = mk_addr ip ~port in
            return
              { game_id = id
              ; game_addr
              ; game_name = name
              ; game_nb_players = players
              ; game_max_players = max_players
              }
        | _ ->
            fail (Error "Ill-typed game parameters dictionary.")
      with Not_found ->
        fail (Error "Missing key in game parameters dictionary.")
    end
    | _ -> fail (Error ("Unexpected non-dictionary value passed as game "
          ^ "parameters."))

  let encode_server = let open Bencode in function
    | ADDED id -> L [ S "ADDED"; I id ]
    | GAMES games ->
        L (S "GAMES" :: List.map bencode_game games)

  let decode_server = let open Bencode in function
    | L [ S "ADDED"; I id ] -> return (ADDED id)
    | L (S "GAMES" :: q) ->
        Lwt_list.map_p bdecode_game q >>= fun games ->
        return (GAMES games)
    | _ -> fail (Error "Invalid message from Protocol.Meta server.")

  module Server = struct
    type input = client
    type output = server

    let input_of_stream s =
      read_stream
        decode_client
        (function
           | Bencode.Format_error e ->
               Lwt_log.error
                ("Bencode format error while decoding messages from " ^
                "Protocol.Meta client: " ^ e)
           | Error where ->
               Lwt_log.error where
           | exn -> fail exn)
        s

    let stream_of_output v = Bencode.to_stream (encode_server v)
  end

  module Client = struct
    type input = server
    type output = client

    let input_of_stream s =
      read_stream
        decode_server
        (function
           | Bencode.Format_error e ->
               Lwt_log.error
                 ("Bencode format error while decoding messages from " ^
                 "Protocol.Meta server: " ^ e)
           | Error where ->
               Lwt_log.error where
           | exn -> fail exn)
        s

    let stream_of_output v = Bencode.to_stream (encode_client v)
  end
end

module Initialisation = struct
  type params =
    { p_game_time : int
    ; p_bomb_time : int
    ; p_bomb_dist : int
    ; p_map_width : int
    ; p_map_height : int
    ; p_turn_time : int
    ; p_start_delay : int
    ; p_version : int
    }

  let bencode_params p =
    D (map_of_list
      [ ("GameTime", I p.p_game_time)
      ; ("BombExplosionTime", I p.p_bomb_time)
      ; ("BombExplosionDistance", I p.p_bomb_dist)
      ; ("MapWidth", I p.p_map_width)
      ; ("MapHeight", I p.p_map_height)
      ; ("TurnTime", I p.p_turn_time)
      ; ("StartDelay", I p.p_start_delay)
      ; ("ProtocolVersion", I p.p_version)
      ])

  let bdecode_params = let open Bencode in function
    | D tbl -> begin
        try
          match gets ["GameTime"; "BombExplosionTime"; "BombExplosionDistance";
                      "MapWidth"; "MapHeight"; "TurnTime"; "StartDelay";
                      "ProtocolVersion" ] tbl with
          | [I gt; I bet; I bed; I mw; I mh; I tt; I sd; I pv] ->
	      return
              { p_game_time = gt
              ; p_bomb_time = bet
              ; p_bomb_dist = bed
              ; p_map_width = mw
              ; p_map_height = mh
              ; p_turn_time = tt
              ; p_start_delay = sd
              ; p_version = pv
              }
          | _ -> failwith "bdecode_params"
        with Not_found -> failwith "bdecode_params"
    end
    | _ -> failwith "becode_params"

  type client = 
    | HELLO of string * int list

  type server =
    | REJECTED of string
    | OK of string * Data.map * params
    | JOIN of string * string * char
    | START of Unix.tm * int
    | QUIT of string

  let bencode_client = let open Bencode in function
    | HELLO (pseudo, versions) ->
        L [ S "HELLO"; S pseudo; L (List.map (fun i -> I i) versions) ]

  let bdecode_client = let open Bencode in function
    | L [ S "HELLO"; S pseudo; L bversions ] ->
        let versions =
	  Lwt_list.map_p
            (function
              | I i when i > 0 -> return i
              | _ -> failwith "bdecode_client")
            bversions
	in versions >>= fun v -> return @$ HELLO (pseudo, v)
    | _ -> failwith "bdecode_client"

  let bencode_server = let open Bencode in function
    | REJECTED reason -> L [ S "REJECTED"; S reason ]
    | OK (id, map, params) ->
        L [ S "OK"; S id; S (Data.string_of_map map); bencode_params params ]
    | JOIN (pseudo, id, pos) ->
        L [ S "JOIN"; S pseudo; S id; S (String.make 1 pos) ]
    | START (date, nano) ->
        let open Unix in
        let { tm_sec ; tm_min ; tm_hour ; tm_mday ; tm_mon ; tm_year ; _ } =
          date in
        let str = Format.sprintf
          "%04d-%02d-%02d %02d:%02d:%02d:%04d"
          (1900 + tm_year) tm_mon tm_mday tm_hour tm_min tm_sec nano in
        L [ S "START"; S str ]
    | QUIT ident ->
        L [ S "QUIT"; S ident ]

  let bdecode_server = let open Bencode in function
    | L [ S "REJECTED"; S reason ] -> return @$ REJECTED reason
    | L [ S "OK"; S id; S smap; bparams ] ->
	bdecode_params bparams >>= fun p ->
        return @$ OK (id, Data.map_of_string smap, p)
    | L [ S "JOIN"; S pseudo; S id; S pos ] (* when *) ->
      (* String.length pos = 1 && pos.[0] >= 'A' && pos.[0] <= 'Z' -> *)
        return @$ JOIN (pseudo, id, pos.[0])
    | L [ S "START"; S strdate ] ->
        let open Unix in
        begin try
          return @$
          Scanf.sscanf strdate "%04d-%02d-%02d %02d:%02d:%02d:%04d"
            (fun tm_year tm_mon tm_mday tm_hour tm_min tm_sec nano ->
              let raw = { tm_sec; tm_min; tm_hour; tm_mday; tm_mon;
              tm_year = tm_year - 1900; tm_yday = 0; tm_wday = 0;
              tm_isdst = false } in
              START (snd (Unix.mktime raw), nano))
        with Scanf.Scan_failure _ -> failwith "bdecode_server start date" end
    | L [ S "QUIT"; S ident ] ->
        return @$ QUIT ident
    | _ -> failwith "bdecode_server"

  module Server = struct
    type input = client
    type output = server

    let input_of_stream =
      read_stream
        bdecode_client
        (function
           | Bencode.Format_error e ->
               Lwt_log.error
                ("Bencode format error while decoding messages from " ^
                "Protocol.Init client: " ^ e)
           | Error where ->
               Lwt_log.error where
           | exn -> fail exn)

    let stream_of_output v = Bencode.to_stream (bencode_server v)
  end

  module Client = struct
    type input = server
    type output = client

    let input_of_stream =
      read_stream
        bdecode_server
        (function
           | Bencode.Format_error e ->
               Lwt_log.error
                ("Bencode format error while decoding messags from " ^
                "Protocol.Init server: " ^ e)
           | Error where ->
               Lwt_log.error where
           | exn -> fail exn)

    let stream_of_output v = Bencode.to_stream (bencode_client v)
  end

end

module Game = struct
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
end
