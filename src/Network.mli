(* Abstract datatype representing addresses *)
type addr

(* Creates an addres from a port and an hostname. Currently, the host name can
 * either be an IPv4 adress in string format X.Y.Z.T, or a domain name. In the
 * second case, [parse_addr] will try to ask DNS servers for resolution of the
 * domain. An optional port number can be appended to the host name by adding a
 * colon followed by the port number. If no port number is given, it defaults
 * to TODO[6669]. If [parse_addr] fails to resolve its argument to a valid
 * address, it raises Not_found.
 *)
val parse_addr : string -> addr Lwt.t

(* [mk_addr] provides another interface to create addresses, and separates the
 * hostname and the port in its argument. See [parse_addr] for more
 * information.
 *)
val mk_addr : ?port:int -> string -> addr Lwt.t

(* Gives a printable representation of an address, of the form hostname:port.
 * [string_of_addr] currently asks DNS server for a reverse DNS entry. *)
val string_of_addr : addr -> string Lwt.t

(* Gets the address and port from an address. *)
val raw_addr : addr -> string * int

module type CHANNEL = sig
    type input
    type output

    val input_of_stream : char Lwt_stream.t -> input option Lwt.t

    val stream_of_output : output -> char Lwt_stream.t
end

module TCP : sig
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
        ?close:(client -> unit) -> addr ->
        (client -> input Lwt_stream.t -> output Lwt_stream.t)  ->
        Lwt_io.server
  end

  module Make(Chan : CHANNEL) : S with
    type input = Chan.input and
    type output = Chan.output
end

module UDP : sig 
  type t

  val create : ?addr:addr -> unit -> t

  val close : t -> unit Lwt.t

  val sendto : t -> string -> addr -> bool

  val recvfrom : t -> (string * addr) option Lwt.t
end
