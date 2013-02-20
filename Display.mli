(** Provides an interface for devices, modules that can interact with the player
 * in the different stages of the protocols.
 * They can represent interaction from a terminal, a graphical interface, an
 * artificial intelligence, ...
 *)

(* TODO: finish documentation. *)

(** Signature shared by all devices *)
module type S = sig
  (** Signature for devices configured to work as a Meta client. *)
  module Meta : sig
    (** Internal type of the device *)
    type t

    (** [create ()] creates a new device able to be used as a Meta client. *)
    val create : unit -> t Lwt.t

    (** [update device games] refreshes the device to display the games in
     * [games].
     *)
    val update : t -> Protocol.Meta.game list -> unit

    (** [input device] waits for a choice of game from the device. 
     * @return [Some game] if a game [game] was chosen by the user, [None] if
       * the user has closed the device. *)
    val input : t -> Protocol.Meta.game option Lwt.t

    (** [quit device] cleanly exits the device [device]. *)
    val quit : t -> unit Lwt.t
  end

  (** Signature for devices configured to work as an Init client *)
  module Init : sig
    (** Internal type of the device *)
    type t

    (** [create ?meta game] creates a new device able to be used as a
     * Initialisation client for the game [game].  If [meta] is provided, the
     * device should reuse the internal state of [meta].
     *)
    val create : ?meta:Meta.t -> Protocol.Meta.game -> t Lwt.t

    (** [input device] waits for interaction on the device.
     * @return [()] if the user has closed the device. *)
    val input : t -> unit Lwt.t

    (** [quit device] cleanly exits the device [device]. *)
    val quit : t -> unit Lwt.t
  end

  (* Signature for devices configured to work as a Game client *)
  module Game : sig
    (** Internal type of the device *)
    type t

    val create : ?init:Init.t -> (string, string * char) Hashtbl.t -> Data.map ->
      int -> string -> t Lwt.t

    (** [update device turn] updates the device [device] turn number to [turn].
     *)
    val update : t -> int -> unit

    (** [input device] waits for interaction on the device.
     * @return [Some command] if the user has issued a command through the
     * device, [None] if he has closed the device.
     *)
    val input : t -> Protocol.Game.client_command option Lwt.t

    (** [quit device] cleanly exits the device [device]. *)
    val quit : t -> unit Lwt.t
  end
end

(** Wrapper to encapsulate any Meta device. *)
module Meta : sig
  type t

  val create : (module S) -> t Lwt.t

  val update : t -> Protocol.Meta.game list -> unit

  val input : t -> Protocol.Meta.game option Lwt.t

  val quit : t -> unit Lwt.t
end

(** Wrapper to encapsulate any Init device. *)
module Init : sig
  type t

  val create : (module S) -> Protocol.Meta.game -> t Lwt.t

  val of_meta : Meta.t -> Protocol.Meta.game -> t Lwt.t

  val input : t -> unit Lwt.t

  val quit : t -> unit Lwt.t
end

(** Wrapper to encapsulate any Game device. *)
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
