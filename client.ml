open Lwt
open Misc

let mport = ref 22222
let meta = ref "127.0.0.1"

let sfml = ref false

let spec =
  [ "--address", Arg.Set_int mport, " Address of the game server [127.0.0.1]"
  ; "--port", Arg.Set_string meta, " Port of the game server [22222]"
  ; "--sfml", Arg.Set sfml, " Use graphical client [false]"
  ]

let pseudo = ref None

let anon s =
  pseudo := Some s

let usage = "usage: " ^ Sys.argv.(0) ^ " [--meta-port <meta port>] "
  ^ "[--meta <meta address>] [--address <address>] [--port <port>] <pseudo>"

let _ =
  Lwt_main.run (
    Arg.parse
      (Arg.align spec)
      anon
      usage;

    match !pseudo with
    | None -> begin
      prerr_endline
        (Sys.argv.(0) ^ ": No pseudo provided");
      Arg.usage spec usage;
      exit 2
    end
    | Some pseudo -> Lwt_unix.handle_unix_error (fun () ->
        let render =
          if !sfml then
            Display.Game.create (module LTermDisplay : Display.S)
          else
            Display.Game.create (module LTermDisplay : Display.S) in
        try_lwt
          let open InitialisationClient in
          Network.mk_addr ~port:!port !addr >>= fun addr ->
          match_lwt hello ~pseudo ~versions:[1] addr with
          | Closed -> return ()
          | Rejected reason -> Lwt_log.error reason
          | Ok { ident; map; params; players; spectators; _ } ->
              Game.Client.main render addr map params players spectators ident
        with Not_found -> Lwt_log.fatal "Unknown address." >> exit 2)
        ()
  )
