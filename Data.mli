type map_case =
  | Empty
  | Indestructible
  | Destructible
  | Bomb of int

type map = private
  { width : int
  ; height : int
  ; content : map_case array array
  ; players : (char, (int * int)) Hashtbl.t
  }

exception InvalidMap

val map_of_string : string -> map

val string_of_map : map -> string
