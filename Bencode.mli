(** Type representing bencodables values *)
type t =
  (** A string *)
  | S of string
  (** An integer *)
  | I of int
  (** A list of bencodables values *)
  | L of t list
  (** A dictionary from strings to bencodables values *)
  | D of t Map.Make(String).t

(** A bencoded value simply is a string *)
type bencoded = private string

(** This exception is raised whenever a badly formatted input is given to the
 * [of_stream] and [of_string] functions. *)
exception Format_error of string

(** Decodes a value from the beginning of a stream of characters. [of_stream]
 * stops once the first complete value is encountered, call it again to get
 * subsequent values. *)
val of_stream : char Lwt_stream.t -> t option Lwt.t

(** Converts a bencodable value to a (finite) stream of characters. *)
val to_stream : t -> char Lwt_stream.t

(** Decodes a value from the beginning of a string. If there is junk at the end
 * of the string, it is ignored, and only the value parsed from the beginning of
 * the string is returned. *)
val of_string : string -> t option Lwt.t

(** Converts a bencodable value to a string *)
val to_string : t -> string

val most_to_string : int -> t list -> string * int

val all_to_strings : int -> t list -> string list
