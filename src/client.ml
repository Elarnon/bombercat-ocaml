open Protocol.Meta
open Protocol.Initialisation

let address = ref None
let sfml = ref false
let direct = ref false
let pseudo = ref None

let spec = Arg.align
  [ "--direct", Arg.Set direct,
    " Treat <address> as an initialisation server (bypass meta protocol)"
  ; "--meta", Arg.Clear direct,
    " Treat <address> as a meta server (default)"
  ; "--sfml", Arg.Set sfml, " Use a graphical client (incomplete)"
  ; "--term", Arg.Clear sfml, " Use a terminal client (default)"
  ]

let anon =
  let nb = ref 0 in
  fun s ->
    if !nb = 0 then begin
      nb := 1;
      address := Some s
    end else if !nb = 1 then begin
      nb := 2;
      pseudo := Some s
    end else
      raise (Arg.Bad "Too many arguments.")

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [--direct] [--sfml] <address>[:<port>] pseudo"

let _ = Lwt_main.run begin
  Arg.parse
    spec
    anon
    usage;

  match !pseudo, !address with
  | None, _ -> begin
    lwt () = Lwt_log.fatal "A pseudo is required." in
    Arg.usage spec usage;
    exit 2
  end
  | _, None -> begin
    lwt () = Lwt_log.fatal "No server address provided." in
    Arg.usage spec usage;
    exit 2
  end
  | Some pseudo, Some address -> Lwt_unix.handle_unix_error (fun () ->
    let render = (module LTermDisplay : Display.S) in (* TODO: SFML *)
    try_lwt
      lwt addr = Network.parse_addr address in
      lwt display = Display.Meta.init render in
      match_lwt MetaClient.run display addr with
      | None -> Display.Meta.free display >> Display.quit render
      | Some ({ game_addr; _ } as game) ->
          Display.Meta.free display >>
          lwt init = Display.Init.init render game in
          let open InitialisationClient in
          match_lwt hello init ~pseudo ~versions:[1] game_addr with
          | Closed -> Display.Init.free init
          | Rejected reason -> Lwt_log.error reason
          | Ok { ident; map; params; players; (*spectators;*) _ } ->
              Display.Init.free init >>
              lwt display =
                Display.Game.init render players map params.p_game_time ident
              in
              GameClient.main
                display game_addr map params players (*spectators*) ident >>
              Display.Game.free display
    with Not_found ->
      Lwt_log.fatal "Unable to connect to the server." >> exit 2
    finally Display.quit render
) () end
