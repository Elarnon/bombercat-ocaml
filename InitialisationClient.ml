open Protocol.Initialisation
open Misc
open Lwt

module Connection = Network.TCP.Make(Protocol.Initialisation.Client)

type event =
  [ `Start of Unix.tm * int
  | `Join of string * string * char
  | `Spectator of string * string
  | `Quit of string
  | `Closed ]

type t =
  Connection.t * Lwt_mutex.t * event Lwt_stream.t *
  [ `Rejected of string
  | `Ok of string * Data.map * Protocol.Initialisation.params ] Lwt_stream.t

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

let hello (srv, mut, _, stream) ~pseudo ~versions =
  Lwt_mutex.with_lock mut (fun () ->
    Connection.send srv (HELLO (pseudo, versions));
    Lwt_stream.get stream)

let spectator (srv, mut, _, stream) ~pseudo =
  Lwt_mutex.with_lock mut (fun () ->
    Connection.send srv (SPECTATOR pseudo);
    Lwt_stream.get stream)

let poll (_, _, stream, _) = Lwt_stream.get stream
