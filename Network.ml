open Lwt

module U = Lwt_unix

type addr = U.sockaddr

let dns_resolve addr =
  (* TODO: Not_found ??? *)
  U.gethostbyname addr >>= fun host ->
  let l = host.U.h_addr_list in
  if Array.length l = 0
  then fail Not_found (* TODO *)
  else return l.(Random.int (Array.length l))

let mk_addr ?(port=6669) addr =
  dns_resolve addr >>= fun ip ->
  return (U.ADDR_INET (ip, port))

let parse_addr s =
  let l = String.length s in
  let i =
    try String.rindex s ':' with Not_found -> l
  in
  let addr = String.sub s 0 i in
  let port =
    if i = l then None
    else try
      Some (int_of_string (String.sub s (i + 1) (l - i - 1)))
    with Failure "int_of_string" -> raise Not_found
  in mk_addr ?port addr

let string_of_addr addr =
  let open U in
  getnameinfo addr [] >>= fun ni ->
  return (ni.ni_hostname ^ ":" ^ ni.ni_service)

let raw_addr addr =
  let open U in
  match addr with
  | ADDR_INET (inet, port) ->
      (Unix.string_of_inet_addr inet, port)
  | _ -> assert false

module type CHANNEL = sig
    type input
    type output

    val input_of_stream : char Lwt_stream.t -> input option Lwt.t

    val stream_of_output : output -> char Lwt_stream.t
end

module TCP = struct

  exception Connection_closed

  module type S = sig
    type input
    type output
    type client
    type t
    type server

    val send : t -> output -> unit Lwt.t

    val recv : t -> input option Lwt.t

    val open_connection : addr -> t Lwt.t

    val create_server : addr -> server

    val join_event : server -> client Lwt_react.E.t
    
    val leave_event : server -> client Lwt_react.E.t

    val shutdown_server : server -> unit

    val read_client : server -> client -> input option Lwt.t

    val write_client : server -> client -> output -> unit

    val broadcast_client : server -> output -> unit

    val reject_client : server -> client -> unit
  end

  module Make(Chan : CHANNEL) = struct

    type input = Chan.input
    type output = Chan.output
    type client = int
    type t = Lwt_io.input_channel * Lwt_io.output_channel
    type server =
      (client, input Lwt_stream.t * (output -> unit Lwt.t) * (unit -> unit Lwt.t))
      Hashtbl.t * Lwt_io.server * client Lwt_react.E.t * client Lwt_react.E.t

    let send (_, output) v =
      Lwt_io.write_chars output (Chan.stream_of_output v)

    let recv (input, _) =
      Chan.input_of_stream (Lwt_io.read_chars input)

    let open_connection addr =
      Lwt_io.open_connection addr

    let create_server addr =
      let cli_id = ref 0 in
      let table = Hashtbl.create 17 in
      let join, send_join = Lwt_react.E.create ()
      and leave, send_leave = Lwt_react.E.create () in
      let server = Lwt_io.establish_server addr
        (fun (input, output) ->
          let id = !cli_id in
          cli_id := id + 1;
          let close () =
            try_lwt
              Hashtbl.remove table id;
              send_leave id;
              Lwt_io.close input >>
              Lwt_io.close output
            with Unix.Unix_error (Unix.ENOTCONN, _, _) -> return () in
          let input_stream =
            Lwt_stream.from (fun () ->
              Lwt_io.atomic (fun inp ->
                Chan.input_of_stream (Lwt_io.read_chars inp)) input >>= function
                  | Some x -> return (Some x)
                  | None -> close () >> return None)
          and push v =
            Lwt_io.atomic
              (fun out -> Lwt_io.write_chars out (Chan.stream_of_output v))
              output
          in Hashtbl.add table id (input_stream, push, close);
          send_join id) in
      (table, server, join, leave)

  let join_event (_, _, join, _) = join

  let leave_event (_, _, _, leave) = leave

  let shutdown_server (_, server, _, _) =
    Lwt_io.shutdown_server server

  let read_client (table, _, _, _) id =
    try
      let (stream, _, _) = Hashtbl.find table id in
      Lwt_stream.get stream
    with Not_found -> return None

  let write_client (table, _, _, _) id v =
    Lwt.async (fun () -> try
      let (_, push, _) = Hashtbl.find table id in
      push v
    with Not_found -> fail Connection_closed)

  let broadcast_client (table, _, _, _) v =
    let threads = Hashtbl.fold
      (fun _ (_, push, _) streams -> push v :: streams)
      table
      []
    in Lwt.async (fun () -> Lwt.join threads)

  let reject_client (table, _, _, _) id =
    try
      let (_, _, close) = Hashtbl.find table id in
      Lwt.async close
    with Not_found -> ()

  end

end
(*
module type Read = sig
  type t
  val read : string -> t
end

module type Show = sig
  type t
  val show : t -> string
end

module type Bufferize = sig
  type t
  type buffer
  val empty_buffer : unit -> buffer
  val add_buffer : buffer -> string -> unit
  val consume_buffer : buffer -> t option
  val reset_buffer : buffer -> unit
end

module BufferizeRead(Data : Read) = struct
  type t = Data.t
  type buffer = Buffer.t
  let empty_buffer () = Buffer.create 16
  let add_buffer = Buffer.add_string
  let consume_buffer buffer =
    let c = Buffer.contents buffer in
    match (try Some (String.index c ':') with Not_found -> None) with
    | Some i -> begin
    match (try Some (int_of_string (String.sub c 0 i))
           with Invalid_argument "int_of_string" -> None) with
       | Some len ->
           let blen = String.length c in
           if blen > len + i + 1 then begin
             Buffer.reset buffer;
             Buffer.add_substring buffer c (len + i + 2) (blen - len - i - 2);
             if c.[len + i + 1] = ',' then
               Some (Data.read c)
             else None
           end else None
       | None -> None
    end
    | None -> None
  let reset_buffer buff = Buffer.reset buff
end

module BufferizeShow(Data : Show) = struct
  type t = Data.t
  let show data =
    let s = Data.show data in
    let len = String.length s in
    string_of_int len ^ ":" ^ s ^ ","
end

module type TCP = sig
  type ('a, 'b) channel
  type 'a input_channel = ('a, Lwt_io.input) channel
  type 'b output_channel = ('b, Lwt_io.output) channel

  val mk_connection :
    addr -> (char Lwt_stream.t -> 'a Lwt.t) -> ('b -> char Lwt_stream.t) ->
      ('a input_channel * 'b output_channel) Lwt.t

  val read : 'a input_channel -> 'a Lwt.t

  val write : 'b output_channel -> 'b -> unit Lwt.t

  val close : ('a, 'b) channel -> unit Lwt.t

  type input
  type output
  type 'a chan
  type read_result =
    | Message of input
    | EOF
    | Nothing
  val mk_server : addr -> [ `Server ] chan
  val mk_client : addr -> [ `In | `Out ] chan
  val check_ops :
    ?accept:([> `Server ] chan list) ->
    ?read:([> `In ] chan list) ->
    ?write:([> `Out ] chan list) ->
    unit -> unit
  val accept : [> `Server ] chan -> [ `In | `Out ] chan option
  val read : [> `In ] chan -> read_result
  val write : [> `Out ] chan -> output -> unit
  val flush : [> `Out ] chan -> unit
  val shutdown : 'a chan -> unit
end

module type UDP = sig
  type input
  type output
  type 'a socket
  val mk_socket : addr -> [ `In | `Out ] socket
  val check_ops :
    ?read:([> `In ] socket list) ->
    ?write:([> `Out ] socket list) ->
    unit -> unit
  val read : [> `In ] socket -> (input * addr) option
  val write : [> `Out ] socket -> output -> addr -> unit
  val flush : [> `Out ] socket -> unit
end

let memo f =
  (* let (tbl : (Unix.file_descr, 'a chan) Hashtbl.t) = Hashtbl.create 17 in *)
  let tbl = Hashtbl.create 17 in
  fun arg ->
    try Hashtbl.find tbl arg
    with Not_found -> begin
      let res = f arg in
      Hashtbl.add tbl arg res;
      res
    end

module TCP = struct

  let mk_server reader writer addr f =
      Lwt_io.establish_server addr
        (fun (input, output) ->
            f (reader (Lwt_io.read_chars in_chan))

  let mk_connection addr reader writer =
    Lwt_io.open_connection addr >>= fun (in_chan, out_chan) ->
    return
        ( Lwt_stream.from (fun () -> reader (Lwt_io.read_chars in_chan))
        , write out_chan )

  let read { reader; channel; _ } = reader (Lwt_io.read_chars channel)

  let read_all channel =
      Lwt_stream.from (fun () -> read channel)

  let write { writer; channel; _ } v = Lwt_io.write_chars channel (writer v)

  let close { channel; _ } = Lwt_io.close channel

end
(*
  type input = In.t

  type output = Out.t

  type 'a chan =
    { sock : Unix.file_descr
    ; mutable can_read : bool
    ; rbuff : In.buffer
    ; mutable can_write : bool
    ; wbuff : Buffer.t
    }

  type read_result = 
    | Message of In.t
    | EOF
    | Nothing

  let chan_of_file =
    memo (fun sock ->
      { sock
      ; can_read = false
      ; rbuff = In.empty_buffer ()
      ; can_write = false
      ; wbuff = Buffer.create 16 })

  let file_of_chan { sock; _ } = sock

  let mk_socket domain =
    chan_of_file Unix.(socket domain SOCK_STREAM 0)

  let bind ({ sock; _ } as chan) addr =
    let open Unix in
    setsockopt sock SO_REUSEADDR true;
    bind sock addr;
    set_nonblock sock;
    listen sock 3;
    chan

  let connect ({ sock; _ } as chan) addr =
    Unix.connect sock addr; chan

  let mk_server addr =
    let domain = Unix.domain_of_sockaddr addr in
    bind (mk_socket domain) addr

  let mk_client addr =
    let domain = Unix.domain_of_sockaddr addr in
    connect (mk_socket domain) addr

  (* Calls select on the underlying file_descr, then mark the chans with the
   * appropriate value.
   * As [Unix.select] counts server sockets as "ready to read" when they, in
   * fact, are "ready to accept", we merge the [accept] and [read] lists and
   * use the same field `can_read' for both. *)
  let check_ops ?(accept=[]) ?(read=[]) ?(write=[]) () =
    let to_read =
      List.fold_left
        (fun r s -> s :: r)
        (List.map file_of_chan read)
        (List.map file_of_chan accept)
    and to_write = List.map file_of_chan write
    in let r, w, _e = Unix.select to_read to_write [] 0.0 in
    List.iter
      (fun s -> (chan_of_file s).can_read <- true)
      r;
    List.iter
      (fun s -> (chan_of_file s).can_write <- true)
      w;
    ()

  let accept ({ sock; can_read; _ } as chan) =
    if can_read then begin
      let cli_sock, _ = Unix.accept sock in
      chan.can_read <- false;
      Some (chan_of_file cli_sock)
    end else None
    
  let read ({ sock; rbuff; can_read; _ } as chan) =
    if can_read then
      let sbuff = String.make 1024 '$' in
      let n = Unix.recv sock sbuff 0 1024 [] in
      chan.can_read <- false;
      if n = 0 then EOF else begin
        prerr_endline "Receiving via TCP: ";
        prerr_string (String.sub sbuff 0 n); prerr_newline ();
        In.add_buffer rbuff (String.sub sbuff 0 n);
        match In.consume_buffer rbuff with
            | Some msg -> Message msg
            | None -> Nothing
       end
    else match In.consume_buffer rbuff with
    | Some msg -> Message msg
    | None -> Nothing

  let write { wbuff; _ } m =
    Buffer.add_string wbuff (Out.show m)

  let flush ({ sock; wbuff; can_write; _ } as chan) =
    if can_write then
    let msg = Buffer.contents wbuff in
    Buffer.reset wbuff;
    if msg <> "" then begin
    prerr_endline "Sending via TCP: ";
    prerr_string msg; prerr_newline () end;
    let _n = Unix.send sock msg 0 (String.length msg) [] in
    chan.can_write <- false

  let shutdown ({ sock; _ } as chan) =
    Unix.shutdown sock Unix.SHUTDOWN_ALL;
    Unix.close sock;
    chan.can_read <- false;
    chan.can_write <- false;
    In.reset_buffer chan.rbuff;
    Buffer.reset chan.wbuff
  
end *)

module MakeUDP(In : Read)(Out : Show) = struct
  type input = In.t

  type output = Out.t

  type 'a socket = 
    { sock : Unix.file_descr
    ; mutable can_read : bool
    ; mutable can_write : bool
    ; wbuff : (string * addr) Queue.t
    }

  let socket_of_file =
    memo (fun sock ->
      { sock
      ; can_read = false
      ; can_write = false
      ; wbuff = Queue.create ()
      })

  let file_of_socket { sock; _ } = sock

  let mk_socket addr =
    let domain = Unix.domain_of_sockaddr addr in
    let socket = socket_of_file Unix.(socket domain SOCK_DGRAM 0) in
    Unix.bind socket.sock addr;
    socket

  let check_ops ?(read=[]) ?(write=[]) () =
    let to_read = List.map file_of_socket read
    and to_write = List.map file_of_socket write
    in let r, w, _e = Unix.select to_read to_write [] 0.0 in
    List.iter
      (fun s -> (socket_of_file s).can_read <- true)
      r;
    List.iter
      (fun s -> (socket_of_file s).can_write <- true)
      w;
    ()

  let read socket =
    (* 65536 is the maximum length of an UDP packet *)
    let udp_len = 65536 in
    let buf = String.make udp_len ' ' in
    (fun ({ sock; can_read; _ } as socket) ->
      if can_read then begin
        let len, addr = Unix.recvfrom sock buf 0 udp_len [] in
        let res = String.sub buf 0 len in
        socket.can_read <- false;
        Some (In.read res, addr)
      end else None) socket

  let write { wbuff; _ } data addr =
    Queue.push (Out.show data, addr) wbuff

  let flush ({ sock; can_write; wbuff; _ } as socket) =
    if can_write && not (Queue.is_empty wbuff) then begin
      let msg, addr = Queue.pop wbuff in
      let _n = Unix.sendto sock msg 0 (String.length msg) [] addr in
      socket.can_write <- false
    end else ()
end
*)
