type 'a t

type mark

val create : unit -> 'a t

val register : 'a t -> ('a -> unit) -> mark

val unregister : mark -> unit

val notify : 'a t -> 'a -> unit
