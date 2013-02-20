open Lwt

let meta = ref "127.0.0.1:22222"
let address = ref None

let spec = Arg.align
  [ "--meta", Arg.String (fun m -> meta := m),
    " The address of the meta server."
  ]

let anon =
  let nb = ref 0 in
  fun s ->
    if !nb = 0 then begin
      nb := 1;
      address := Some s
    end else
      raise (Arg.Bad "Too many arguments.")

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [--meta <address>[:<port>]] <address>:[<port>]"

let _ = Lwt_main.run begin
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
      lwt meta_addr =
        (* match !meta with
        | None -> return None
        | Some m ->*) Network.parse_addr !meta in
      lwt init_addr = Network.parse_addr addr in
      MetaClient.Connection.open_connection meta_addr >>= fun meta ->
      InitialisationServer.run init_addr meta >>= function
        | None -> return_unit
        | Some game ->
            GameServer.run init_addr game
    with Not_found -> Lwt_log.fatal "Bad address." >> exit 2
  ) ()
end
