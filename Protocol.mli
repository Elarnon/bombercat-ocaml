exception Error of string

module Meta : sig
  type game =
    { game_id : int
    ; game_addr : Network.addr
    ; game_name : string
    ; game_nb_players : int
    ; game_max_players : int
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

module Game : sig
  type dir =
    | UP | RIGHT | DOWN | LEFT

  type pos = int * int

  type client_command =
    | MOVE of pos * dir
    | BOMB of pos

  type client_raw =
    | RAW_COMMAND of client_command
    | RAW_SYNC of int

  type client =
    | COMMAND of string * int * int * client_command
    | SYNC of string * int

  type action =
    | CLIENT of client_raw
    | NOP of int
    | DEAD

  type stats =
    { winner : string option }

  type server =
    | TURN of int * action list Map.Make(String).t
    | GAMEOVER of int * stats
end 
