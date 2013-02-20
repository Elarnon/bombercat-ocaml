open Lwt

exception Device_in_use

exception Invalid_resource

module type S = sig
  module Meta : sig
    type t

    val init : unit -> t Lwt.t

    val update : t -> Protocol.Meta.game list -> unit

    val error : t -> string -> unit

    val input : t -> Protocol.Meta.game option Lwt.t

    val free : t -> unit Lwt.t
  end

  module Init : sig
    type t

    val init : Protocol.Meta.game -> t Lwt.t

    val input : t -> unit Lwt.t

    val free : t -> unit Lwt.t
  end

  module Game : sig
    type t

    val init : (string, string * char) Hashtbl.t -> Data.map ->
    int -> string -> t Lwt.t

    val update : t -> int -> unit

    val input : t -> Protocol.Game.client_command option Lwt.t

    val free : t -> unit Lwt.t
  end

  val quit : unit -> unit Lwt.t
end

module type META = sig
  module D : S
  val x : D.Meta.t
end

module type INIT = sig
  module D : S
  val x : D.Init.t
end

module type GAME = sig
  module D : S
  val x : D.Game.t
end

module Meta = struct
  type t = (module META)

  let init d =
    let module D = (val d : S) in
    lwt x = D.Meta.init () in
    return (module struct
      module D = D
      let x = x
    end : META)

  let update m =
    let module M = (val m : META) in
    M.D.Meta.update M.x

  let error m =
    let module M = (val m : META) in
    M.D.Meta.error M.x

  let input m =
    let module M = (val m : META) in
    M.D.Meta.input M.x

  let free m =
    let module M = (val m : META) in
    M.D.Meta.free M.x
end

module Init = struct 
  type t = (module INIT)

  let init d game =
    let module D = (val d : S) in
    lwt x = D.Init.init game in
    return (module struct
      module D = D
      let x = x
    end : INIT)

  let input i =
    let module I = (val i : INIT) in
    I.D.Init.input I.x

  let free i =
    let module I = (val i : INIT) in
    I.D.Init.free I.x
end

module Game = struct
  type t = (module GAME)

  let init d h m t i =
    let module D = (val d : S) in
    lwt x = D.Game.init h m t i in
    return (module struct
      module D = D
      let x = x
    end : GAME)

  let update g =
    let module G = (val g : GAME) in
    G.D.Game.update G.x

  let input g =
    let module G = (val g : GAME) in
    G.D.Game.input G.x

  let free g =
    let module G = (val g : GAME) in
    G.D.Game.free G.x
end

let quit d =
  let module D = (val d : S) in
  D.quit ()
