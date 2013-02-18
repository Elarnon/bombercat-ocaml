open Lwt

let port = ref 22222

let spec =
  [ "--port", Arg.Set_int (port), "The (TCP) port the meta server should listen to."
  ]

let ip = ref "0.0.0.0"

let anon s =
  ip := s

let usage = "./meta_server [--port <bind port>] [<bind address>]"

let _ =
  Arg.parse
    spec
    anon
    usage;

  Lwt_main.run (
    Lwt_unix.handle_unix_error (fun () ->
    try_lwt
      Network.mk_addr ~port:!port !ip >>= fun meta_addr ->
      let meta_server = Meta.Server.create meta_addr in
      let stream = Lwt_io.read_chars Lwt_io.stdin in
      let rec eat () =
        Lwt_stream.get stream >>= function
          | Some _ -> eat ()
          | None -> return ()
      in eat () >>= fun () ->
      Meta.Server.shutdown meta_server;
      return_unit
    with Not_found -> Lwt_log.fatal "Bad address." >> exit 2
  ) ())
