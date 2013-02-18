open Lwt
open Misc

let mport = ref 22222
let port = ref 12345
let meta = ref "127.0.0.1"
let addr = ref "127.0.0.1"

let spec =
  [ "--port", Arg.Set_int (port), "The port of the game server to connect to."
  ; "--meta-port", Arg.Set_int mport, "The TCP port of the meta server to connect to"
  ; "--meta", Arg.Set_string meta, "The address of the meta server to connect to"
  ; "--address", Arg.Set_string addr, "The address of the game server to connect to"
  ]

let pseudo = ref "elarnon"

let anon s =
  pseudo := s

let usage = "./meta_server [--port <port>] [--meta-port <meta port>] "
  ^ "[--meta <meta address>] [--address <address>] <pseudo>"

let _ =
  Arg.parse
    spec
    anon
    usage;

  Lwt_main.run (
    Lwt_unix.handle_unix_error (fun () ->
    try_lwt
      Network.mk_addr ~port:!port !addr >>= fun init_addr ->
      let pseudo = !pseudo in
      lwt co = Initialisation.Client.connect init_addr in
      lwt res = Initialisation.Client.hello co ~pseudo ~versions:[1] in
      match res with
      | None -> return ()
      | Some (`Rejected reason) -> Lwt_log.error reason
      | Some (`Ok (ident, map, params)) ->
          let players = Hashtbl.create 17 in
          let rec loop () =
            Initialisation.Client.poll co >>= function
              | None -> return ()
              | Some (`Join (pseudo, ident, map_id)) ->
                  Hashtbl.replace players ident (pseudo, map_id);
                  Lwt_log.debug "JOIN" >>
                  loop ()
              | Some (`Quit ident) ->
                  Hashtbl.remove players ident;
                  Lwt_log.debug "QUIT" >>
                  loop ()
              | Some `Closed -> return ()
              | Some (`Start (_, _)) ->
                  Game.Client.main init_addr map params players ident
          in loop ()
    with Not_found -> Lwt_log.fatal "Bad address." >> exit 2
    ) ())
