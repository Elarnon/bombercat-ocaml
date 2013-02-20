open Lwt

let port = ref 12345
let mport = ref 22222
let meta = ref "127.0.0.1"

let spec =
  [ "--port", Arg.Set_int (port), "The port the initialisation and game servers should listen to."
  ; "--meta-port", Arg.Set_int (mport), "The port to connect to on the meta server."
  ; "--meta", Arg.Set_string meta, "The address of the meta server."
  ]

let ip = ref "127.0.0.1"

let anon s =
  ip := s

let usage = "./game_server [--meta-port <meta port>] [--meta <meta address>]"
  ^ " [--port <bind port>] [<bind address>]"

let _ =
  Arg.parse
    spec
    anon
    usage;

  Lwt_main.run (
    Lwt_unix.handle_unix_error (fun () ->
    try_lwt
      Network.mk_addr ~port:!mport !meta >>= fun meta_addr ->
      Network.mk_addr ~port:!port !ip >>= fun init_addr ->
      MetaClient.Connection.open_connection meta_addr >>= fun meta ->
      InitialisationServer.run init_addr meta >>= function
        | None -> return_unit
        | Some game ->
            GameServer.run init_addr game
    with Not_found -> Lwt_log.fatal "Bad address." >> exit 2
    ) ())
