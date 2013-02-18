open Lwt

module type S = sig
  type t

  val create : Data.map -> Protocol.Initialisation.params -> char -> t Lwt.t

  (* Give turn number *)
  val update : t -> int -> unit

  val input : t -> Protocol.Game.client_command option Lwt.t

  val quit : t -> unit Lwt.t
end

module type T = sig
  module D : S

  val v : D.t
end

type t = (module T)

let create display map params id =
  let module Display = (val display : S) in
  lwt v = Display.create map params id in
  let module Wrapped = struct
    module D = Display
    let v = v
  end in
  return (module Wrapped : T)

let update m =
  let module Wrap = (val m : T) in
  Wrap.(D.update v)

let input m =
  let module Wrap = (val m : T) in
  Wrap.(D.input v)

let quit m =
  let module Wrap = (val m : T) in
  Wrap.(D.quit v)
