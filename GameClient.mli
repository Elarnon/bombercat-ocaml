val main :
  Display.Game.t -> Network.addr -> Data.map -> Protocol.Initialisation.params
  -> (string, (string * char)) Hashtbl.t -> string -> unit Lwt.t
