(** Module implementing an integer-labelled queue with reordering *)

(** The type of integer-labelled queues of elements of type ['a] *)
type 'a t

(** Raised when trying to take elements from an empty queue *)
exception Empty

(** Creates a new integer-labelled queue.
 * @param window The reordering window of the queue. If not specified,
 * arbitrarily big holes in the queue will be possible. Specifying a value will
 * help prevent memory overflows.
 * @param max_size The maximum initial, consecutive queue size. If not
 * specified, it can be arbitrarily big. Specifying a value will help prevent
 * queue saturation.
 *)
val create : ?window:int -> ?max_size:int -> unit -> 'a t

(** [add queue label value] adds the value [value] into the queue [queue] with
 * label [label].
 * @return [true] if [value] was correctly added to the queue, and [false] if
 * adding it with label [label] would overflow the queue window.
 *)
val add : 'a t -> int -> 'a -> bool

(** Resets the queue buffer (any elements after the initial consecutive values
 * will be discarded). Moreover, label counter is reinitialized to [0].
 * @param notify If specified, adds the result of [notify last_label] to the
 * queue, where [last_label] is the last label that was not discarded.
 *)
val interrupt : ?notify:(int -> 'a) -> 'a t -> unit

(** [peek queue] returns the top element in the [queue] without removing it.
 * @raise Empty if [queue] is empty
 *)
val peek : 'a t -> 'a

(** [take queue] removes the top element from [queue] and returns it.
 * @raise Empty if [queue] is empty
 *)
val take : 'a t -> 'a

(** [junk queue] removes the top element from [queue] and discards it.
 * @raise Empty if [queue] is empty
 *)
val junk : 'a t -> unit

(** [last_id queue] returns the highest label in the initial consecutive segment
 * of [queue]. *)
val last_id : 'a t -> int

(** [is_full queue] tests whether the initial consecutive segment of [queue]
 * reaches the queue [max_size].
 *)
val is_full : 'a t -> bool
