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

module Make(D : S) = struct
  let create map params id =
    lwt v = D.create map params id in
    let module Wrap = struct
      module D = D
      let v = v
    end in
    return (module Wrap : T)
end

let update m =
  let module Wrap = (val m : T) in
  Wrap.(D.update v)

let input m =
  let module Wrap = (val m : T) in
  Wrap.(D.input v)

let quit m =
  let module Wrap = (val m : T) in
  Wrap.(D.quit v)
