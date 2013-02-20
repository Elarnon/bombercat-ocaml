(** Module representing core game data, that is, maps *)

(** The type of cases on the map *)
type case =
  | Empty
  | Destructible
  | Indestructible
  | Bomb of int (** A bomb with the time before its explosion *)

(** The type of maps *)
type map

(** A position on the map *)
type pos = int * int

(** A direction *)
type dir = Left | Right | Up | Down

(** Raised when trying to read an invalid map *)
exception InvalidMap

(** Raised when an internal inconsistency in the [Data] module is detected. This
 * exception should never be raised, and is just provided to help identify
 * functions that can fail due to [Data] module programmer's errors. The
 * behavior of the program if subsequent treatment is done with maps after they
 * caused this exception to be raied is undefined. *)
exception Inconsistency of string

(** Reads a map from a string.
 * @raise InvalidMap if the map is not in the format specified by the protocol
 *)
val map_of_string : string -> map

(** [iter_contnt f map] calls [f] on each position of the map [map] with its
 * content.
 * @raise Inconsistency on internal inconsistency *)
val iter_content : (pos -> case -> unit) -> map -> unit

(** [iter_players f map] calls [f] on each player present on [map] with its
 * position. Note that [f] may be called on players that have no associated
 * clients in the game. *)
val iter_players : (char -> pos -> unit) -> map -> unit

(** Dumps a map into a string.
 * @raise Inconsistency on internal inconsistency *)
val string_of_map : map -> string

(** Returns an unspecified player present on the map, or none if there is no
 * more players on the map. *)
val map_choose : map -> char option

(** Returns the number of players on the map. *)
val map_nb_players : map -> int

(** Returns the position of a player on the map.
 * @raise Not_found if the player is not present on the map *)
val map_pos : map -> char -> pos

(** Returns the list of all players on a given position. *)
val map_players : map -> pos -> char list

(** Returns the map's width. *)
val width : map -> int

(** Returns the map's height. *)
val height : map -> int

(** Returns whether a position on the map is valid (i.e., not out of bounds) *)
val is_pos_valid : map -> pos -> bool

(** [try_bomb map player pos timer dist] tries to make player [player] to put a
 * bomb with timer [timer] and explosion distance [dist] at position [pos].
 * @return [true] if a bomb was placed, [false] if it was invalid to put such a
 * bomb. *)
val try_bomb : map -> char -> pos -> int -> int -> bool

(** [try_move map player pos dir] tries to make player [player] move from
 * position [pos] in direction [dir].
 * @return [true] if the move was issued, [false] if it was invalid. *)
val try_move : map -> char -> pos -> dir -> bool

(** Decreases bomb timers and returns a list of players that were killed.
 * @raise Inconsistency on internal inconsistency *)
val decrease_timers : map -> char list
(* TODO: make it return explosion infos *)
