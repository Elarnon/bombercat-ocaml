type game =
  { g_map : Data.map
  ; g_params : Protocol.Initialisation.params
  ; g_players : (string, string * char) Hashtbl.t
  ; g_start : Unix.tm * int
  ; g_name : string
  }

(* type server *)

val create : Network.addr -> MetaClient.Connection.t -> game option Lwt.t

(* val shutdown : server -> unit *)
