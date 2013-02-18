module type S = sig
  type t

  val create : Data.map -> Protocol.Initialisation.params -> char -> t Lwt.t

  (* Give turn number *)
  val update : t -> int -> unit

  val input : t -> Protocol.Game.client_command option Lwt.t

  val quit : t -> unit Lwt.t
end

type t

val create :
  (module S) -> Data.map -> Protocol.Initialisation.params -> char -> t Lwt.t

val update : t -> int -> unit

val input : t -> Protocol.Game.client_command option Lwt.t

val quit : t -> unit Lwt.t
