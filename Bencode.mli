type t =
  | String of string
  | Integer of int
  | List of t list
  | Dict of (t, t) Hashtbl.t
  
type bencoded = private string

(* type bytes = private string *)

exception Format_error of string * int

val of_string : string -> t

val decode : bencoded -> t

val encode : t -> bencoded

(* val encode_bytes : t -> bytes

val decode_bytes : bytes -> t *)
