type obj =
  | Empty
  | Indestructible
  | Destructible
  | Bomb of int
  
type pos =
  | Pos of int * int
  | Dead
  
type player =
  { pos : pos }
  
type world =
  { map : obj array array
  ; players : player array
  }
  
val read_world : string -> world

val display_world : world -> unit

val string_of_map : world -> string

val map_of_string : string -> world