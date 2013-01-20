open Lwt
open Misc

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

  module type S = sig
    type input
    type output
    type client
    type t

    val send : t -> output -> unit

    val recv : t -> input option Lwt.t

    val open_connection : addr -> t Lwt.t

    val close : t -> unit

    val establish_server :
        ?close:(client -> unit Lwt.t) -> addr
        -> (client -> input Lwt_stream.t -> output Lwt_stream.t)
        -> Lwt_io.server
  end

  module Make(Chan : CHANNEL) = struct

    type input = Chan.input
    type output = Chan.output
    type client = int
    type t =
      Lwt_io.input_channel * Lwt_io.output_channel *
      (unit -> unit Lwt.t) list ref

    let send (_, output, _) v =
      Lwt.async (fun () ->
        Lwt_io.atomic (fun chan ->
          Lwt_io.write_chars chan (Chan.stream_of_output v)) output
      )

    let close (inp, outp, l) =
      Lwt.async (fun () ->
        Lwt_io.close outp >>
        Lwt_io.close inp
      )

    let recv ((input, _, _) as t) =
      try_lwt
        Chan.input_of_stream (Lwt_io.read_chars input)
      with
        End_of_file -> close t; return_none

    let open_connection addr =
      Lwt_io.open_connection addr >>= fun (inp, outp) ->
      return (inp, outp, ref [])

    let establish_server
      ?(close=fun _ -> return ())
      addr
      f =
      let cli_id = ref 0 in
      Lwt_io.establish_server addr
        (fun (input, output) ->
          Lwt.ignore_result begin
            let id = !cli_id in
            cli_id := id + 1;
            let stream = Lwt_stream.from (fun () ->
              Chan.input_of_stream (Lwt_io.read_chars input)) in
              Lwt_stream.fold_s (fun v () ->
                Lwt_io.write_chars output
                  (Chan.stream_of_output v))
              (f id stream) () >>= fun () ->
                catch (fun () ->
                  Lwt_io.close output >>
                  Lwt_io.close input >>
                  return id)
                (function
                  | Unix.Unix_error (Unix.ENOTCONN, _, _) -> return id
                  | e -> fail e) >>= close
          end)
  end

end

module UDP = struct

  module type S = sig
    type input
    type output
    type t

    val create : ?addr:addr -> unit -> t

    val sendto : t -> output -> addr -> bool Lwt.t

    val recvfrom : t -> (input * addr) option Lwt.t
  end

  module Make(Chan : CHANNEL) = struct

    type input = Chan.input
    
    type output = Chan.output

    type t = U.file_descr

    let create ?(addr=U.ADDR_INET (Unix.inet_addr_any, 0)) () =
      U.socket Unix.PF_INET Unix.SOCK_DGRAM 0

    let close = U.close

    let sendto fdescr out addr =
      Lwt_stream.to_string (Chan.stream_of_output out) >>= fun str ->
      let len = String.length str in
      if len > 65536 then return false else begin
        Lwt.async (wrap_eintr (fun () ->
          match U.state fdescr with
          | U.Opened -> U.sendto fdescr str 0 len [] addr >> return ()
          | U.Aborted _ -> U.close fdescr (* TODO ? *)
          | U.Closed -> return ()));
        return true
      end

    let rec recvfrom fdescr =
      let buffer = String.make 65536 ' ' in
      wrap_eintr (fun () ->
        match U.state fdescr with
        | U.Opened ->  U.recvfrom fdescr buffer 0 65536 [] >|= fun x -> Some x
        | U.Aborted _ -> U.close fdescr >> return None (* TODO ? *)
        | U.Closed -> return None) () >>= function
          | None -> return None
          | Some (len, addr) -> begin
              let stream = Lwt_stream.of_string (String.sub buffer 0 len) in
              Chan.input_of_stream stream >>= function
                | None -> recvfrom fdescr (* Ignore invalid packets *)
                | Some input -> return @$ Some (input, addr)
          end

  end

end
