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

  module type S = sig
    type input
    type output
    type client
    type t

    val send : t -> output -> unit Lwt.t

    val recv : t -> input option Lwt.t

    val open_connection : addr -> t Lwt.t

    val establish_server :
        ?close:(client -> unit Lwt.t) -> addr
        -> (client -> input Lwt_stream.t -> output Lwt_stream.t)
        -> Lwt_io.server
  end

  module Make(Chan : CHANNEL) = struct

    type input = Chan.input
    type output = Chan.output
    type client = int
    type t = Lwt_io.input_channel * Lwt_io.output_channel

    let send (_, output) v =
      Lwt_io.write_chars output (Chan.stream_of_output v)

    let recv (input, _) =
      Chan.input_of_stream (Lwt_io.read_chars input)

    let open_connection addr =
      Lwt_io.open_connection addr

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
                  Lwt_io.close output >>= fun () ->
                  return id)
                (function
                  | Unix.Unix_error (Unix.ENOTCONN, _, _) -> return id
                  | e -> fail e) >>= close
          end)
  end

end
