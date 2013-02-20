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

(** [of_stream stream] decodes a value from the beginning of a stream of
 * characters. [of_stream] stops once the first complete value is encountered,
 * call it again to get subsequent values. 
 * @param stream The stream (supposedly) containing the encoded value
 * @return [Some b] When the stream actually contains an encoded value
 * @return [None] When the stream is empty
 * @raise Format_error When the stream does not contain an encoded value
 *)
val of_stream : char Lwt_stream.t -> t option Lwt.t

(** [to_stream b] converts a Bencode value to a (finite) stream of characters.
 * @param b The Bencode value
 * @return A stream of characters representing the value [b]
 *)
val to_stream : t -> char Lwt_stream.t

(** [of_string str] decodes a value from the beginning of a string. If there is
 * junk at the end of the string, it is ignored, and only the value parsed from
 * the beginning of the string is returned.
 * @param str The string (supposedly) containing an encoded value
 * @return [Some b] When the string actually is a prefix of an encoded value
 *         [b].
 * @return [None] When the string is empty
 * @raise Format_error When the string is not a prefix of an encoded value
 *)
val of_string : string -> t option

(** [to_string b] bonverts a Bencode value to a string
 * @param b The Bencode value
 * @return A string of characters representing the value [b] 
 *)
val to_string : t -> string

(** [most_to_string len lst] converts as many bencode velues to a string of
 * length at most [len] as possible, and returns both that string and the number
 * of bencoded values that was converted. 
 * @param len The maximum length of the string
 * @param lst All the values to serialize
 * @return A couple [(str, nb)] where [str] is a string of length at most [len]
 *         representing the [nb] firsts elements of [lst].
 *)
val most_to_string : int -> t list -> string * int

(** [all_to_strings len lst] converts all bencoded values to as many strings of
 * length at most [len] as needed, and returns those.  [all_to_strings] loops
 * infinitely when a value in [lst] serializes to a string of length greater
 * than [len].
 * @param len The maximum length of the string
 * @param lst All the values to serialize
 * @return A list of strings of length at most [len] representing the elements
 *         of [lst].
 *)
val all_to_strings : int -> t list -> string list
