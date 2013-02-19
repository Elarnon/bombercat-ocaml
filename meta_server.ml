open Lwt

let port = ref 22222

let ip = ref "0.0.0.0"

let spec =
  [ "--port", Arg.Set_int port, " TCP port to bind [22222]."
  ; "--address", Arg.Set_string ip, " IP address to bind [0.0.0.0]."
  ]

let exec = Sys.argv.(0)

let anon s =
  raise (Arg.Bad (s ^ ": Invalid argument"))

let usage = "usage: " ^ exec ^ " [--port <bind port>] [--address <bind address>]"

let _ =
  Lwt_main.run (
    Arg.parse
      (Arg.align spec)
      anon
      usage;

    Lwt_unix.handle_unix_error (fun () ->
      try_lwt
        lwt meta_addr = Network.mk_addr ~port:!port !ip in
        let meta_server = Meta.Server.create meta_addr in
        let stream = Lwt_io.read_chars Lwt_io.stdin in
        let rec eat () =
          Lwt_stream.get stream >>= function
            | Some _ -> eat ()
            | None -> return ()
        in eat () >>= fun () ->
        Meta.Server.shutdown meta_server;
        return_unit
      with Not_found -> Lwt_log.fatal (!ip ^ ": Invalid address") >> exit 2)
      ()
  )
