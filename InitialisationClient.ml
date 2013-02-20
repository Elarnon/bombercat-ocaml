open Protocol.Initialisation
open Misc
open Lwt

module Connection = Network.TCP.Make(Protocol.Initialisation.Client)

type data =
  { ident : string
  ; map : Data.map
  ; params : params
  ; players : (string, (string * char)) Hashtbl.t
  ; spectators : (string, string) Hashtbl.t
  ; start : Unix.tm * int
  }

type result =
  | Rejected of string
  | Ok of data
  | Closed

let connect addr =
  Connection.open_connection addr >>= fun co ->
  let stream, push = Lwt_stream.create () in
  let stream_hello, push_hello = Lwt_stream.create () in
  let rec loop () =
    Connection.recv co >>= function
      | None -> push_hello None; push None; return ()
      | Some (REJECTED s) -> push_hello @$ Some (`Rejected s); loop ()
      | Some (OK (s, m, p)) -> push_hello @$ Some (`Ok (s, m, p)); loop ()
      | Some (START (date, nano)) ->
          push @$ Some (`Start (date, nano)); loop ()
      | Some (JOIN (pseudo, id, pos)) ->
          push @$ Some (`Join (pseudo, id, pos)); loop ()
      | Some (SPECTATORJOIN (pseudo, id)) ->
          push @$ Some (`Spectator (pseudo, id)); loop ()
      | Some (QUIT id) ->
          push @$ Some (`Quit id); loop ()
  in
    Lwt.async loop;
    return (co, Lwt_mutex.create (), stream, stream_hello)

let send_and_wait (srv, mut, _, stream) msg =
  Lwt_mutex.with_lock mut (fun () ->
    Connection.send srv msg;
    Lwt_stream.get stream)

let poll (_, _, stream, _) = Lwt_stream.get stream

let init msg addr =
  lwt co = connect addr in
  match_lwt send_and_wait co msg with
  | None -> return Closed
  | Some (`Rejected reason) -> return (Rejected reason)
  | Some (`Ok (ident, map, params)) ->
      let players = Hashtbl.create 17 in
      let spectators = Hashtbl.create 17 in
      let rec loop () =
        match_lwt poll co with
        | None -> return Closed
        | Some (`Join (pseudo, ident, map_id)) ->
            Hashtbl.replace players ident (pseudo, map_id);
            loop ()
        | Some (`Spectator (pseudo, ident)) ->
            Hashtbl.replace spectators ident pseudo;
            loop ()
        | Some (`Quit ident) ->
            Hashtbl.remove players ident;
            Hashtbl.remove spectators ident;
            loop ()
        | Some (`Start start) ->
            return @$ Ok { ident; map; params; players; spectators; start }
      in loop ()

let hello ~pseudo ?(versions=[1]) =
  init (HELLO (pseudo, versions))

let spectator ~pseudo =
  init (SPECTATOR pseudo)
