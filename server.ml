open Lwt
  
let _ =
  Lwt_main.run
    ((Network.parse_addr "127.0.0.1:22222") >>=
       (fun meta_addr ->
          let meta_server = Meta.Server.create meta_addr
          in
            (Meta.Client.Connection.open_connection meta_addr) >>=
              (fun meta ->
                 (Network.parse_addr "127.0.0.1:12345") >>=
                   (fun init_addr ->
                      (Initialisation.Server.create init_addr meta) >>=
                        (fun game ->
                           (Game.Server.main init_addr game) >>=
                             (fun () ->
                                (Meta.Server.shutdown meta_server;
                                 return_unit)))))))
  

