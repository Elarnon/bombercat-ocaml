type map

type pos = int * int

type dir = Left | Right | Up | Down

exception InvalidMap

val map_of_string : string -> map

val string_of_map : map -> string

val players : map -> (char, pos) Hashtbl.t

val width : map -> int

val height : map -> int

val is_pos_valid : map -> pos -> bool

val try_bomb : map -> char -> pos -> int -> int -> bool

val try_move : map -> char -> pos -> dir -> bool

val decrease_timers : map -> char list

val display : map -> unit Lwt.t
