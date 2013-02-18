type 'a t

exception Empty

val create : ?window:int -> ?max_size:int -> int -> 'a t

val add : 'a t -> int -> 'a -> bool

val interrupt : ?notify:(int -> 'a) -> 'a t -> unit

val peek : 'a t -> 'a

val take : 'a t -> 'a

val junk : 'a t -> unit

val last_id : 'a t -> int

val is_full : 'a t -> bool
