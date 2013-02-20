open Lwt

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

  let create d =
    let module D = (val d : S) in
    lwt x = D.Meta.create () in
    return (module struct
      module D = D
      let x = x
    end : META)

  let update m =
    let module M = (val m : META) in
    M.D.Meta.update M.x

  let input m =
    let module M = (val m : META) in
    M.D.Meta.input M.x

  let quit m =
    let module M = (val m : META) in
    M.D.Meta.quit M.x
end

module Init = struct 
  type t = (module INIT)

  let create d game =
    let module D = (val d : S) in
    lwt x = D.Init.create game in
    return (module struct
      module D = D
      let x = x
    end : INIT)

  let of_meta m game =
    let module M = (val m : META) in
    lwt init = M.D.Init.create ~meta:M.x game in
    return (module struct
      module D = M.D
      let x = init
    end : INIT)

  let quit i =
    let module I = (val i : INIT) in
    I.D.Init.quit I.x
end

module Game = struct
  type t = (module GAME)

  let create d h m t i =
    let module D = (val d : S) in
    lwt x = D.Game.create h m t i in
    return (module struct
      module D = D
      let x = x
    end : GAME)

  let of_init i h m t id =
    let module I = (val i : INIT) in
    lwt gmod = I.D.Game.create ~init:I.x h m t id in
    return (module struct
      module D = I.D
      let x = gmod
    end : GAME)

  let update g =
    let module G = (val g : GAME) in
    G.D.Game.update G.x

  let input g =
    let module G = (val g : GAME) in
    G.D.Game.input G.x

  let quit g =
    let module G = (val g : GAME) in
    G.D.Game.quit G.x
end
