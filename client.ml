open Lwt
open Misc

let mport = ref 22222
let meta = ref "127.0.0.1"

let sfml = ref false

let spec =
  [ "--address", Arg.Set_int mport, " Address of the meta server [127.0.0.1]"
  ; "--port", Arg.Set_string meta, " Port of the meta server [22222]"
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
    | Some pseudo ->
      Lwt_unix.handle_unix_error (fun () ->
        try_lwt
          Network.mk_addr ~port:!port !addr >>= fun init_addr ->
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
                      let display =
                        if !sfml then
                          Display.create (module OcsfmlDisplay : Display.S)
                        else
                          Display.create (module LTermDisplay : Display.S)
                      in
                      Game.Client.main display init_addr map params players ident
              in loop ()
        with Not_found -> Lwt_log.fatal "Unknown address." >> exit 2)
        ()
  )
