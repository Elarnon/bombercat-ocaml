open Lwt
  
open Misc
  
let _ =
  Lwt_main.run
    ((Network.parse_addr "127.0.0.1:12345") >>=
       (fun init_addr ->
          (Random.self_init ();
           let pseudo = char_of_int ((int_of_char 'A') + (Random.int 26)) in
           let pseudo = String.make 1 pseudo in
           let __pa_lwt_0 = Initialisation.Client.connect init_addr
           in
             Lwt.bind __pa_lwt_0
               (fun co ->
                  let __pa_lwt_0 =
                    Initialisation.Client.hello co ~pseudo ~versions: [ 1 ]
                  in
                    Lwt.bind __pa_lwt_0
                      (fun res ->
                         match res with
                         | None -> return ()
                         | Some (`Rejected reason) -> Lwt_log.error reason
                         | Some (`Ok (ident, map, params)) ->
                             let players = Hashtbl.create 17 in
                             let rec loop () =
                               (Initialisation.Client.poll co) >>=
                                 (function
                                  | None -> return ()
                                  | Some (`Join (pseudo, ident, map_id)) ->
                                      (Hashtbl.replace players ident
                                         (pseudo, map_id);
                                       Lwt.bind (Lwt_log.debug "JOIN")
                                         (fun _ -> loop ()))
                                  | Some (`Quit ident) ->
                                      (Hashtbl.remove players ident;
                                       Lwt.bind (Lwt_log.debug "QUIT")
                                         (fun _ -> loop ()))
                                  | Some `Closed -> return ()
                                  | Some (`Start (_, _)) ->
                                      Game.Client.main init_addr map params
                                        players ident)
                             in loop ())))))
  

