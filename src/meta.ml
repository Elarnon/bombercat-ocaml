open Lwt

let address = ref None

let spec = Arg.align
  [
  ]

let exec = Sys.argv.(0)

let anon =
  let nb = ref 0 in
  fun s ->
    if !nb = 0 then begin
      nb := 1;
      address := Some s
    end else
      raise (Arg.Bad "Too many arguments.")

let usage = "usage: " ^ exec ^ " <address>[:<port>]"

let _ =
  Lwt_main.run (
    Arg.parse
      spec
      anon
      usage;

    match !address with
    | None -> begin
      Lwt_log.ign_fatal "An address to bind is required.";
      Arg.usage spec usage;
      exit 2
    end
    | Some addr -> Lwt_unix.handle_unix_error (fun () ->
      try_lwt
        lwt meta_addr = Network.parse_addr addr in
        let meta_server = MetaServer.create meta_addr in
        let stream = Lwt_io.read_chars Lwt_io.stdin in
        let rec eat () =
          Lwt_stream.get stream >>= function
            | Some _ -> eat ()
            | None -> return ()
        in eat () >>= fun () ->
        MetaServer.shutdown meta_server;
        return_unit
      with Not_found -> Lwt_log.fatal "Invalid address." >> exit 2)
      ()
  )
