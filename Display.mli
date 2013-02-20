module type S = sig
  module Meta : sig
    type t

    val create : unit -> t Lwt.t

    val update : t -> Protocol.Meta.game list -> unit

    val input : t -> Protocol.Meta.game option Lwt.t

    val quit : t -> unit Lwt.t
  end

  module Init : sig
    type t

    val create : ?meta:Meta.t -> Protocol.Meta.game -> t Lwt.t

    val quit : t -> unit Lwt.t
  end

  module Game : sig
    type t

  val create : ?init:Init.t -> (string, string * char) Hashtbl.t -> Data.map ->
    int -> string -> t Lwt.t

    val update : t -> int -> unit

    val input : t -> Protocol.Game.client_command option Lwt.t

    val quit : t -> unit Lwt.t
  end
end

module Meta : sig
  type t

  val create : (module S) -> t Lwt.t

  val update : t -> Protocol.Meta.game list -> unit

  val input : t -> Protocol.Meta.game option Lwt.t

  val quit : t -> unit Lwt.t
end

module Init : sig
  type t

  val create : (module S) -> Protocol.Meta.game -> t Lwt.t

  val of_meta : Meta.t -> Protocol.Meta.game -> t Lwt.t

  val quit : t -> unit Lwt.t
end

module Game : sig
  type t

  val create : (module S) -> (string, string * char) Hashtbl.t -> Data.map ->
    int -> string -> t Lwt.t

  val of_init : Init.t -> (string, string * char) Hashtbl.t -> Data.map ->
    int -> string -> t Lwt.t

  val update : t -> int -> unit

  val input : t -> Protocol.Game.client_command option Lwt.t

  val quit : t -> unit Lwt.t
end
