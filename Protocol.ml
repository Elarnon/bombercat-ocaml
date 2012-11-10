open Network

exception ReadError

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
    | L [ S "ADD"; S ip; I port; S name; I nb_players ] ->
        let addr = mk_addr ~port ip in (* TODO: try/catch *)
        if nb_players < 1
        then raise ReadError
        else ADD (addr, name, nb_players)
    | L [ S "UPDATE"; I game_id; I nb_players ] ->
        if nb_players < 1
        then raise ReadError
        else UPDATE (game_id, nb_players)
    | L [ S "DELETE"; I game_id ] ->
        DELETE game_id
    | S "LIST" -> LIST
    | _ -> raise ReadError

  let bencode_game g =
    let open Bencode in
    let (ip, port) = raw_addr g.game_addr in
    let tbl = Hashtbl.create 17 in
    Hashtbl.add tbl (S "Id") (I g.game_id);
    Hashtbl.add tbl (S "Ip") (S ip);
    Hashtbl.add tbl (S "Port") (I port);
    Hashtbl.add tbl (S "Name") (S g.game_name);
    Hashtbl.add tbl (S "Players") (I g.game_nb_players);
    D tbl

  let bdecode_game = let open Bencode in function
    | D tbl -> begin
      try
        let bid = Hashtbl.find tbl (S "Id")
        and bip = Hashtbl.find tbl (S "Ip")
        and bport = Hashtbl.find tbl (S "Port")
        and bname = Hashtbl.find tbl (S "Name")
        and bnb_players = Hashtbl.find tbl (S "Players") in
        match (bid, bip, bport, bname, bnb_players) with
        | I id, S ip, I port, S name, I players when players > 0 ->
            { game_id = id
            ; game_addr = mk_addr ip ~port
            ; game_name = name
            ; game_nb_players = players
            }
        | _ -> raise ReadError
      with Not_found -> raise ReadError
    end
    | _ -> raise ReadError

  let encode_server = let open Bencode in function
    | ADDED id -> L [ S "ADDED"; I id ]
    | GAMES games ->
        L (S "GAMES" :: List.map bencode_game games)

  let decode_server = let open Bencode in function
    | L [ S "ADDED"; I id ] -> ADDED id
    | L (S "GAMES" :: q) ->
        let games = List.map bdecode_game q in
        GAMES games
    | _ -> raise ReadError

  module WClient = struct
    type t = client
    let encode = encode_client
    let decode = decode_client
  end

  module WServer = struct
    type t = server
    let encode = encode_server
    let decode = decode_server
  end

  module Server =
    Network.MakeTCP(Bencode.WrapBufferize(WClient))(Bencode.WrapShow(WServer))

  module Client =
    Network.MakeTCP(Bencode.WrapBufferize(WServer))(Bencode.WrapShow(WClient))
end

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
  let open Bencode in
  let open Hashtbl in
  let tbl = create 17 in
  add tbl (S "GameTime") (I p.p_game_time);
  add tbl (S "BombExplosionTime") (I p.p_bomb_time);
  add tbl (S "BombExplosionDistance") (I p.p_bomb_dist);
  add tbl (S "MapWidth") (I p.p_map_width);
  add tbl (S "MapHeight") (I p.p_map_height);
  add tbl (S "TurnTime") (I p.p_turn_time);
  add tbl (S "StartDelay") (I p.p_start_delay);
  add tbl (S "ProtocolVersion") (I p.p_version);
  D tbl

let bdecode_params = let open Bencode in function
  | D tbl -> begin
      try
        let open Hashtbl in
        match (
          find tbl (S "GameTime"),
          find tbl (S "BombExplosionTime"),
          find tbl (S "BombExplosionDistance"),
          find tbl (S "MapWidth"),
          find tbl (S "MapHeight"),
          find tbl (S "TurnTime"),
          find tbl (S "StartDelay"),
          find tbl (S "ProtocolVersion")
        ) with
        | I gt, I bet, I bed, I mw, I mh, I tt, I sd, I pv ->
            { p_game_time = gt
            ; p_bomb_time = bet
            ; p_bomb_dist = bed
            ; p_map_width = mw
            ; p_map_height = mh
            ; p_turn_time = tt
            ; p_start_delay = sd
            ; p_version = pv
            }
        | _ -> raise ReadError
      with Not_found -> raise ReadError
  end
  | _ -> raise ReadError

module Initialisation = struct
  type client = 
    | HELLO of string * int list

  type server =
    | REJECTED of string
    (* | OK of string * Data.world * params TODO *)
    | JOIN of string * string * char
    | START of string

  let bencode_client = let open Bencode in function
    | HELLO (pseudo, versions) ->
        L [ S "HELLO"; S pseudo; L (List.map (fun i -> I i) versions) ]

  let bdecode_client = let open Bencode in function
    | L [ S "HELLO"; S pseudo; L bversions ] ->
        let versions =
          List.map
            (function
              | I i when i > 0 -> i
              | _ -> raise ReadError)
            bversions
        in HELLO (pseudo, versions)
    | _ -> raise ReadError

  let bencode_server = let open Bencode in function
    | REJECTED reason -> L [ S "REJECTED"; S reason ]
    (* | OK (id, map, params) ->
        L [ S "OK"; S id; S (Data.string_of_map map); bencode_params params ]
TODO *)
    | JOIN (pseudo, id, pos) ->
        L [ S "JOIN"; S pseudo; S id; S (String.make 1 pos) ]
    | START date ->
        L [ S "START"; S date ]

  let bdecode_server = let open Bencode in function
    | L [ S "REJECTED"; S reason ] -> REJECTED reason
    (* | L [ S "OK"; S id; S smap; bparams ] ->
        OK (id, Data.map_of_string smap, bdecode_params bparams)
TODO *)
    | L [ S "JOIN"; S pseudo; S id; S pos ] when
      String.length pos = 1 && pos.[0] >= 'A' && pos.[0] <= 'Z' ->
        JOIN (pseudo, id, pos.[0])
    | L [ S "START"; S date ] ->
        START date
    | _ -> raise ReadError

  module WClient = struct
    type t = client
    let encode = encode_client
    let decode = decode_client
  end

  module WServer = struct
    type t = server
    let encode = encode_server
    let decode = decode_server
  end

  module Server =
    Network.MakeTCP(Bencode.WrapBufferize(WClient))(Bencode.WrapShow(WServer))

  module Client =
    Network.MakeTCP(Bencode.WrapBufferize(WServer))(Bencode.WrapShow(WClient))
end

module Game = struct
  type dir =
    | N | E | S | W
    
  let dir_of_string = function
    | "UP" -> N
    | "RIGHT" -> E
    | "DOWN" -> S
    | "LEFT" -> W
    | _ -> raise ReadError

  let bdecode_dir = function
    | Bencode.S s -> dir_of_string s
    | _ -> raise ReadError
    
  let string_of_dir = function
    | N -> "UP"
    | E -> "RIGHT"
    | S -> "DOWN"
    | W -> "LEFT"

  let bencode_dir d = Bencode.S (string_of_dir d)

  type pos = int * int

  let bdecode_pos = let open Bencode in function
    | L [ I x; I y ] -> (x, y)
    | _ -> raise ReadError

  let bencode_pos (x, y) =
    Bencode.(L [ I x; I y])

  type client =
    | MOVE of pos * dir
    | BOMB of pos
    | SYNC of int

  type server_action =
    | BROADCAST of client
    | NOP of int
    | DEAD

  type server =
    | TURN of int * (string * server_action) list
    | GAMEOVER of int * string option

  let bencode_client = let open Bencode in function
    | MOVE (pos, dir) ->
        L [ S "MOVE"; bencode_pos pos; bencode_dir dir ]
    | BOMB pos ->
        L [ S "BOMB"; bencode_pos pos ]
    | SYNC last ->
        L [ S "SYNC"; I last ]

  let bdecode_client = let open Bencode in function
    | L [ S "MOVE"; bpos; bdir ] ->
        let pos = bdecode_pos bpos
        and dir = bdecode_dir bdir in
        MOVE (pos, dir)
    | L [ S "BOMB"; bpos ] ->
        let pos = bdecode_pos bpos in
        BOMB pos
    | L [ S "SYNC"; I last ] ->
        SYNC last
    | _ -> raise ReadError

  let bencode_server_action = let open Bencode in function
    | BROADCAST client -> bencode_client client
    | NOP i -> L [ S "NOP"; I i ]
    | DEAD -> S "DEAD"

  let bdecode_server_action = let open Bencode in function
    | L [ S "NOP"; I i ] -> NOP i
    | S "DEAD" -> DEAD
    | b -> BROADCAST (bdecode_client b)

  let bencode_server = let open Bencode in function
    | TURN (i, l) ->
        let tbl = Hashtbl.create 17 in
        List.iter
          (fun (k, v) -> Hashtbl.add tbl (S k) (bencode_server_action v))
          l;
        L [ S "TURN"; D tbl ]
    | GAMEOVER (i, s) ->
        let tbl = Hashtbl.create 17 in
        begin match s with
        | None -> ()
        | Some v -> Hashtbl.add tbl (S "Winner") (S v) end;
        L [ S "GAMEOVER"; I i; D tbl ]

  module WClient = struct
    type t = client
    let encode = encode_client
    let decode = decode_client
  end

  module WServer = struct
    type t = server
    let encode = encode_server
    let decode = decode_server
  end

  module Server =
    Network.MakeUDP(Bencode.WrapRead(WClient))(Bencode.WrapShow(WServer))

  module Client =
    Network.MakeUDP(Bencode.WrapRead(WServer))(Bencode.WrapShow(WClient))

end
