open Lwt
open Misc

let _ =
  Lwt_main.run (
    Network.parse_addr "127.0.0.1:12344" >>= fun init_addr ->
    Random.self_init ();
    let pseudo = char_of_int (int_of_char 'A' + Random.int 26) in
    let pseudo = String.make 1 pseudo in
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
  )
