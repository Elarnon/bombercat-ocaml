(** Represent Bencode data type and a way to convert between an OCaml
 *  representation of these values and their usual string representation.
 *)

(** Bencode values *)
type t =
  | S of string
  (** A string *)
  | I of int
  (** An integer *)
  | L of t list
  (** A list of Bencode values *)
  | D of t Map.Make(String).t
  (** A dictionary from strings to Bencode values *)

(** Raised whenever a badly formatted input is given to the [of_stream] and
 *  [of_string] functions. *)
exception Format_error of string

(** [of_stream stream] reads a Bencode value at the beginning of [stream].
 * [of_stream] stops once a complete value is encountered ; you must call it
 * again to get subsequent values.
 * @raise Format_error if [stream] doesn't contain a valid Bencode value. In
 *        that case, no characters are consumed from [stream].
 *)
val of_stream : char Lwt_stream.t -> t option Lwt.t

(** [to_stream b] converts [b] to a finite stream of characters
 *)
val to_stream : t -> char Lwt_stream.t

(** [of_string str] reads a Bencode value from the beginning of a string, and
 * ignores any junk at the end of the string.
 * @raise Format_error if the string isn't a prefix of a Bencode value.
 *)
val of_string : string -> t option

(** [to_string b] converts [b] to a string
 *)
val to_string : t -> string

(** [most_to_string len lst] converts as many Bencode value to a string of
 * length at most [len] as possible, and places them into a list.
 * @return A couple [(str, nb]) where [str] is a string of length at most [len]
 *         containing the concatenation (in the same order) of the [nb] firsts
 *         elements of [lst].
 *)
val most_to_string : int -> t list -> string * int

(** [all_to_strings len lst] converts all Bencode values in [lst] to as many
 * strings of length at most [len] as needed (see {!most_to_string} for more
 * details about each of these strings).
 * @raise Failure ["all_to_strings"] if a value in [lst] serializes to a string
 *        of length greater than [len].
 *)
val all_to_strings : int -> t list -> string list
