type t =
  | S of string
  | I of int
  | L of t list
  | D of (string, t) Hashtbl.t

type bencoded = private string

exception Format_error of string

val of_stream : char Lwt_stream.t -> t option Lwt.t

val to_stream : t -> char Lwt_stream.t

val of_string : string -> t Lwt.t

val to_string : t -> string Lwt.t
