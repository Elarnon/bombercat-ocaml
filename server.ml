open Lwt

let _ =
  Lwt_main.run (
    Network.parse_addr "0.0.0.0:12345" >>= fun addr ->
      let srv = Meta.server addr in
      let t, _ = Lwt.wait () in
      t
  )
