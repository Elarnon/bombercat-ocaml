open Lwt
open Protocol.Meta
open Protocol.Initialisation

let port = ref 22222
let address = ref "127.0.0.1"

let spec =
  [ "--port", Arg.Set_int port, " TCP port of the meta server [22222]"
  ; "--address", Arg.Set_string address, " IP address of the meta server [127.0.0.1]"
  ; "--sfml", Arg.Set sfml, " Use graphical client [false]"
  ]

let anon _ = ()

let usage = "usage: " ^ Sys.argv.(0) ^ " [--port <meta port>] "
            ^ "[--address <meta address>] [--sfml]"

let _ = Lwt_main.run begin Lwt_unix.handle_unix_error (fun () ->
  Arg.parse
    (Arg.align spec)
    anon
    usage;

  try_lwt
    let render = (module LTermDisplay) in (* TODO: SFML *)
    lwt addr = Network.mk_addr ~port:!port !address in
    lwt display = Display.Meta.create render in
    match_lwt MetaClient.run display addr with
    | None -> return ()
    | Some ({ game_addr; _ } as game) ->
        lwt init = Display.Init.of_meta display game in
        let open InitialisationClient in
        let pseudo = "learnon" (* TODO *) in
        match_lwt hello ~pseudo ~versions:[1] game_addr with
        | Closed -> return ()
        | Rejected reason -> Lwt_log.error reason
        | Ok { ident; map; params; players; (*spectators;*) _ } ->
            lwt display =
              Display.Game.of_init init players map params.p_game_time ident in
            GameClient.main
              display game_addr map params players (*spectators*) ident
  with Not_found ->
    Lwt_log.fatal "Unable to connect to the server." >> exit 2
) () end
