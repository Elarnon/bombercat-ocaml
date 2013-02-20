(** Specifies the datatypes used by the different protocols, as well as ways to
 * converts them to and from Bencode values. *)

(** Specifies the datatypes for the Meta protocol. *)
module Meta : sig
  (** Exception raised when a value can't be parsed as a correct protocol
   * message. *)
  exception Error of string

  (** Type representing a game. *)
  type game =
    { game_id : int
    (** Unique identifier of the game. *)
    ; game_addr : Network.addr
    (** Address of the game server. *)
    ; game_name : string
    (** Human-readable name of the game. *)
    ; game_nb_players : int
    (** Current number of players in the game. *)
    ; game_max_players : int
    (** Maximum number of players in the game. *)
    }

  (** Type representing a message from a Meta client. *)
  type client =
    | ADD of Network.addr * string * int
    | UPDATE of int * int
    | DELETE of int
    | LIST

  (** Type representing a message from a Meta server. *)
  type server = 
    | ADDED of int
    | GAMES of game list

  (** [decode_client b] tries to parse the Bencode value [b] as a client message.
   * @raise Error if [b] does not represent a client message.
   *)
  val decode_client : Bencode.t -> client Lwt.t

  (** [encode_client msg] encodes the client message [msg] into a Bencode value.
   *)
  val encode_client : client -> Bencode.t

  (** [decode_server b] tries to decode the Bencode value [b] as a server
   * message.
   * @raise Error if [b] does not represent a server message.
   *)
  val decode_server : Bencode.t -> server Lwt.t

  (** [encode_server msg] encodes the server message [msg] into a Bencode value.
   *)
  val encode_server : server -> Bencode.t

  (** High-level interface for use with [Network.TCP.Make] for the server. *)
  module Server : Network.CHANNEL with
    type input = client and
    type output = server

  (** High-level interface for use with [Network.TCP.Make] for the client. *)
  module Client : Network.CHANNEL with
    type input = server and
    type output = client
end

(** Specifies the datatypes for the Initialisation protocol. *)
module Initialisation : sig
  (** Type representing game parameters. *)
  type params =
    { p_game_time : int
    (** Maximal time (in turns) the game can last. *)
    ; p_bomb_time : int
    (** Time (in turns) after which a bomb explodes. *)
    ; p_bomb_dist : int
    (** Range of bomb explosions. *)
    ; p_map_width : int
    (** Width of the game map. *)
    ; p_map_height : int
    (** Height of the game map. *)
    ; p_turn_time : int
    (** Time, in milliseconds, that a turn lasts. *)
    ; p_start_delay : int
    (** Delay (in milliseconds) after which the game server will start once
     * initialisation is complete. *)
    ; p_version : int
    (** Protocol version of the game protocol used by the server. *)
    }

  (** Type representing a game. *)
  type game =
    { g_map : Data.map
    (** Game map *)
    ; g_params : params
    (** Game parameters *)
    ; g_players : (string, string * char) Hashtbl.t
    (** Mapping from player identifier to player pseudo and map identifier *)
    ; g_start : Unix.tm * int
    (** Game start time *)
    ; g_name : string
    (** Human-readable name for the game *)
    }

  (** Type representing messages from an Initialisation client *)
  type client =
    | HELLO of string * int list
    | SPECTATOR of string

  (** Type representing messages from an Initialisation server *)
  type server =
    | REJECTED of string
    | OK of string * Data.map * params
    | JOIN of string * string * char
    | SPECTATORJOIN of string * string
    | START of Unix.tm * int
    | QUIT of string

  (** High-level interface for use with [Network.TCP.Make] for the server *)
  module Server : Network.CHANNEL with
    type input = client and
    type output = server

  (** High-level interface for use with [Network.TCP.Make] for the client *)
  module Client : Network.CHANNEL with
    type input = server and
    type output = client

end

(** Specifies the datatypes for the Game protocol *)
module Game : sig
  (** Type representing a command from the client *)
  type client_command =
    | MOVE of Data.pos * Data.dir
    | BOMB of Data.pos

  (** Type representing a message from the client *)
  type client =
    | COMMAND of string * int * int * client_command
    | SYNC of string * int

  (** Type representing possible actions in a [TURN] message *)
  type action =
    | CLIENT of client_command
    (** Command from a player that was accepted by the server *)
    | NOP of int
    (** Resynchronisation message *)
    | DEAD
    (** Player is dead *)

  (** Type representing end-of-game statistics *)
  type stats =
    { winner : string option (** Identifier of the winner of the game *) }

  (** Type representing server messages *)
  type server =
    | TURN of int * action list Map.Make(String).t
    | GAMEOVER of int * stats

  (** [most_clients_to_string msgs] converts as many client messages in the list
   * [msgs] into a string fitting an UDP packet as possible.
   * @return [(str, nb)] where [str] is a string fitting an UDP packet
   * representing the [nb] firsts client messages in [msgs].
   *)
  val most_clients_to_string : client list -> string * int

  (** [all_clients_to_strings msgs] converts all client messages in the list
   * [msgs] into as many strings fitting an UDP packet as needed. If any client
   * message doesn't fit an UDP packet (very unlikely), [all_clients_to_strings]
   * loops forever. 
   *)
  val all_clients_to_strings : client list -> string list

  (** [most_servers_to_string msgs] converts as many server messages in the list
   * [msgs] into a string fitting an UDP packet as possible.
   * @return [(str, nb)] where [str] is a string fitting an UDP packet
   * representing the [nb] firsts server messages in [msgs].
   *)
  val most_servers_to_string : ?nops:string -> server list -> string * int

  (** [all_servers_to_string msgs] converts all server messages in the list
   * [msgs] into as many strings fitting an UDP packet as needed. If any server
   * message doesn't fit an UDP packet (very unlikely, unless there is a huge
   * number of clients), [all_clients_to_strings] loops forever. 
   *)
  val all_servers_to_strings : ?nops:string -> server list -> string list

  (** [string_to_clients str] parses the string [str] into a list of client
   * messages. In case of parsing error, an empty list is returned.
   *)
  val string_to_clients : string -> client list

  (** [string_to_servers str] parses the string [str] into a list of server
   * messages. In case of parsing error, an empty list is returned.
   *)
  val string_to_servers : string -> server list
end 
