(** Provides an interface for devices, modules that can interact with the player
 * in the different stages of the protocols.
 * They can represent interaction from a terminal, a graphical interface, an
 * artificial intelligence, ...
 *)

(** Raised when trying to initialize a device already in use *)
exception Device_in_use

(** Raised when trying to use a resource that was previously freed *)
exception Invalid_resource

(** Signature shared by all devices *)
module type S = sig
  (** Signature for devices configured to work as a Meta client. *)
  module Meta : sig
    (** Internal type of the device *)
    type t

    (** [init ()] initializes the device to be used as a Meta client.
     * @raise Device_in_use if the device has already been initaliazed. *)
    val init : unit -> t Lwt.t

    (** [update resource games] refreshes the device to display the games in
     * [games].
     * @raise Invalid_resource if [resource] has been freed.
     *)
    val update : t -> Protocol.Meta.game list -> unit
    
    (** [error resource msg] refreshes the device to show an error message
     * [msg].
     * @raise Invalid_resource if [resource] has been freed. *)
    val error : t -> string -> unit

    (** [input resource] waits for a choice of game from the device. 
     * @return [Some game] if a game [game] was chosen by the user, [None] if
     * the user has closed the device.
     * @raise Invalid_resource if [device] has been freed. *)
    val input : t -> Protocol.Meta.game option Lwt.t

    (** [free resource] cleanly exits the resource [resource] and makes the
     * device usable again. *)
    val free : t -> unit Lwt.t
  end

  (** Signature for devices configured to work as an Init client *)
  module Init : sig
    (** Internal type of the device *)
    type t

    (** [init game] initializes the device to be used as an Initialisation
     * client for the game [game].
     * @raise Device_in_use if the device has already been initialized.
     *)
    val init : Protocol.Meta.game -> t Lwt.t

    (** [input resource] waits for interaction on the device.
     * @return [()] if the user has closed the device.
     * @raise Invalid_resource if [resource] has been freed. *)
    val input : t -> unit Lwt.t

    (** [free resource] frees the resource [resource] and returns makes the
     * device usable again. *)
    val free : t -> unit Lwt.t
  end

  (* Signature for devices configured to work as a Game client *)
  module Game : sig
    (** Internal type of the device *)
    type t

    val init : (string, string * char) Hashtbl.t -> Data.map ->
      int -> string -> t Lwt.t

    (** [update resource turn] updates the resource [resource] turn number to
     * [turn].
     * @raise Invalid_resource if [resource] has been freed. *)
    val update : t -> int -> unit

    (** [input resource] waits for interaction on the device.
     * @return [Some command] if the user has issued a command through the
     * device, [None] if he has closed the device.
     * @raise Invalid_resource if [resource] has been freed. *)
    val input : t -> Protocol.Game.client_command option Lwt.t

    (** [free device] cleanly exits the device [device] and returns a re-usable
     * [resource]. *)
    val free : t -> unit Lwt.t
  end

  (** Exits the device, rendering it unusable. *)
  val quit : unit -> unit Lwt.t
end

(** Wrapper to encapsulate any Meta device. *)
module Meta : sig
  type t

  val init : (module S) -> t Lwt.t

  val update : t -> Protocol.Meta.game list -> unit

  val error : t -> string -> unit

  val input : t -> Protocol.Meta.game option Lwt.t

  val free : t -> unit Lwt.t
end

(** Wrapper to encapsulate any Init device. *)
module Init : sig
  type t 

  val init : (module S) -> Protocol.Meta.game -> t Lwt.t

  val input : t -> unit Lwt.t

  val free : t -> unit Lwt.t
end

(** Wrapper to encapsulate any Game device. *)
module Game : sig
  type t 

  val init : (module S) -> (string, string * char) Hashtbl.t -> Data.map ->
    int -> string -> t Lwt.t

  val update : t -> int -> unit

  val input : t -> Protocol.Game.client_command option Lwt.t

  val free : t -> unit Lwt.t
end

val quit : (module S) -> unit Lwt.t
