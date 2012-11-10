(** Abstract type representing bencoded values for treatment by OCaml *)
type t =
  | S of string
  | I of int
  | L of t list
  | D of (t, t) Hashtbl.t

type bencode = t

type bencoded = private string

exception Format_error

val of_string : string -> t

val to_string : t -> string

val decode : bencoded -> t

val encode : t -> bencoded

module Bufferize : Network.Bufferize with type t = t

module Show : Network.Show with type t = t

module Read : Network.Read with type t = t

module TCP : Network.TCP with type input = t and type output = t

module UDP : Network.UDP with type input = t and type output = t

module type Wrapper = sig
  type t
  val encode : t -> bencode
  val decode : bencode -> t
end

module WrapBufferize :
  functor (W : Wrapper) -> Network.Bufferize with type t = W.t

module WrapShow :
  functor (W : Wrapper) -> Network.Show with type t = W.t

module WrapRead :
  functor (W : Wrapper) -> Network.Read with type t = W.t
