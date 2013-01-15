exception Error of string

module Meta : sig
  type game =
    { game_id : int
    ; game_addr : Network.addr
    ; game_name : string
    ; game_nb_players : int
    }

  type client =
    | ADD of Network.addr * string * int
    | UPDATE of int * int
    | DELETE of int
    | LIST

  type server = 
    | ADDED of int
    | GAMES of game list

  val decode_client : Bencode.t -> client Lwt.t

  val encode_client : client -> Bencode.t

  val decode_server : Bencode.t -> server Lwt.t

  val encode_server : server -> Bencode.t

  module Server : Network.CHANNEL with
    type input = client and
    type output = server

  module Client : Network.CHANNEL with
    type input = server and
    type output = client
end

module Initialisation : sig
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

  type client =
    | HELLO of string * int list

  type server =
    | REJECTED of string
    | OK of string * Data.map * params
    | JOIN of string * string * char
    | START of Unix.tm * int
    | QUIT of string

  module Server : Network.CHANNEL with
    type input = client and
    type output = server

  module Client : Network.CHANNEL with
    type input = server and
    type output = client

end

(*

module Game : sig
  type dir =
    | N | E | S | W

  type pos = int * int

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

  module Server : Network.UDP with
    type input = client and type output = server

  module Client : Network.UDP with
    type input = server and type output = client
end
*)
