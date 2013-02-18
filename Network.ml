open Lwt
open Misc

module U = Lwt_unix

type addr = U.sockaddr

(* Raises Not_found -- TODO ? *)
let dns_resolve addr =
  lwt host = U.gethostbyname addr in
  let l = host.U.h_addr_list in
  let len = Array.length l in
  if len = 0
    then fail Not_found
    else return l.(Random.int len)

let mk_addr ?(port=6669) addr =
  lwt ip =
    try_lwt
      return @$ Unix.inet_addr_of_string addr
    with Failure _ -> dns_resolve addr
  in return @$ U.ADDR_INET (ip, port)

let parse_addr s =
  let l = String.length s in
  let i =
    try String.rindex s ':' with Not_found -> l
  in
  let first_colon =
    try Some (String.index s ':') with Not_found -> None
  in
  let start_port, addr =
    if first_colon = Some i
    then (* IPv4 or domain name - single colon *)
        i, String.sub s 0 i
    else (* IPv6 -- detect if port number is present *)
      if l > 0 && s.[0] = '[' && s.[i - 1] == ']'
      then (* IPv6 with port of the form "[IPv6]:port" *)
        i, String.sub s 1 (i - 1)
      else (* Assume raw IPv6 *)
        l, s
  in let port =
    if start_port = l then None
    else try
      let v = int_of_string (String.sub s (i + 1) (l - i - 1)) in
      if v < 0 then raise Not_found
      else Some v
    with Failure "int_of_string" -> raise Not_found
  in mk_addr ?port addr

(* TODO: check raw IP and not domain name in case of PF_INET6 *)
let string_of_addr addr =
  let open U in
  lwt ni = getnameinfo addr [] in
  match Unix.domain_of_sockaddr addr with
  | PF_INET6 -> return @$ "[" ^ ni.ni_hostname ^ "]:" ^ ni.ni_service
  | _ -> return @$ ni.ni_hostname ^ ":" ^ ni.ni_service

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
        ?close:(client -> unit) -> addr
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
      ?(close=fun _ -> ())
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
                  | e -> fail e) >|= close
          end)
  end

end

module UDP = struct
  type t = U.file_descr

  let create ?(addr=U.ADDR_INET (Unix.inet_addr_any, 0)) () =
    let domain = Unix.domain_of_sockaddr addr in
    let sock = U.socket domain Unix.SOCK_DGRAM 0 in
    U.bind sock addr;
    sock

  let close = U.close

  let _ = Random.self_init ()

  let sendto fdescr str addr =
    let len = String.length str in
    Lwt.async (fun () -> Lwt_log.debug str);
    (* if Random.int 10 < 3 then true else *)
    if len > 65535 then false else begin
      Lwt.async (wrap_eintr (fun () ->
        match U.state fdescr with
        | U.Opened -> U.sendto fdescr str 0 len [] addr >> return ()
        | U.Aborted _ -> U.close fdescr (* TODO ? *)
        | U.Closed -> return ()));
        true
    end

  let rec recvfrom fdescr =
    (* We have to make a new buffer each time because there are potentially
     * multiple instances of [recvfrom] running in parallel *)
    let buffer = String.make 65536 ' ' in
    wrap_eintr (fun () ->
      match U.state fdescr with
      | U.Opened ->  U.recvfrom fdescr buffer 0 65535 [] >|= fun x -> Some x
      | U.Aborted _ -> U.close fdescr >> return None (* TODO ? *)
      | U.Closed -> return None) () >|= function
        | None -> None
        | Some (len, addr) -> Some (String.sub buffer 0 len, addr)
end
