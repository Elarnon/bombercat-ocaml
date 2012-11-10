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
val parse_addr : string -> addr

(* [mk_addr] provides another interface to create addresses, and separates the
 * hostname and the port in its argument. See [parse_addr] for more
 * information.
 *)
val mk_addr : ?port:int -> string -> addr

(* Gives a printable representation of an address, of the form hostname:port.
 * [string_of_addr] currently asks DNS server for a reverse DNS entry. *)
val string_of_addr : addr -> string

(* Gets the address and port from an address. *)
val raw_addr : addr -> string * int

module type Read = sig
  (* Readable type *)
  type t

  (* Converts a serialized value from a text channel to an OCaml value *)
  val read : string -> t
end

module type Show = sig
  (* Showable type *)
  type t

  (* Converts an OCaml value to a string representation for sending over a text
   * channel *)
  val show : t -> string
end

module type Bufferize = sig
  (* Buffered type *)
  type t

  (* Type of the buffer *)
  type buffer

  (* Empty buffer value. This buffer should always return None when
   * passed as an argument to [consume_buffer]. *)
  val empty_buffer : unit -> buffer

  (* Adds a string to the buffer. *)
  val add_buffer : buffer -> string -> unit

  (* Try to read a value from the buffer if enough data is already available
   * for that. *)
  val consume_buffer : buffer -> t option
end

(* Creates a Bufferizable instance from a Serializable instance by encoding the
 * serialized strings in netstring format. *)
module BufferizeRead :
  functor (Data : Read) -> Bufferize with type t = Data.t

module BufferizeShow :
  functor (Data : Show) -> Show with type t = Data.t

module type TCP = sig
  (* Type of the data this kind of connection receives *)
  type input

  (* Type of the data this kind of TCP connection sends *)
  type output

  (* Abstract type to represent channels, with a phantom type parameter *)
  type 'a chan

  (* Result of a read, which may be either some message, nothing, or a special
   * value indicating the end of the connection *)
  type read_result =
    | Message of input
    | EOF
    | Nothing

  (* Internally creates a socket and binds it to make a server listening on the
   * given address *)
  val mk_server : addr -> [ `Server ] chan

  (* Internally creates a socket and connects it to the given address *)
  val mk_client : addr -> [ `In | `Out ] chan

  (* Check for available operations on a channel.
   * Channels in one of the accept, read or write list are marked as ready for
   * the corresponding operation when they are.
   * Internally calls [Unix.select] *)
  val check_ops :
    ?accept:([> `Server ] chan list) ->
    ?read:([> `In ] chan list) ->
    ?write:([> `Out ] chan list) ->
    unit -> unit

  (* Accept incoming clients to a server socket if a previous call to
   * `check_ops' marked the socket as having available connections *)
  val accept : [> `Server ] chan -> [ `In | `Out ] chan option

  (* Buffered read. A true read will be done iff the channel was marked as
   * ready to be read by a previous call to `check_ops'. In that case, [read]
   * may throw any exception [Data.add_buffer] or [Data.consume_buffer] throw.
   *)
  val read : [> `In ] chan -> read_result

  (* Buffered write *)
  val write : [> `Out ] chan -> output -> unit

  (* Flush output buffer iff the channel was marked as ready for writing by a
   * previous call to [check_ops] (and does nothing otherwise). In that case,
   * [flush] may throw any exception [Data.to_string] throws. *)
  val flush : [> `Out ] chan -> unit

end

module type UDP = sig
  (* Type of the data this kind of UDP socket receives *)
  type input

  (* Type of the data this kind of UDP socket sends *)
  type output

  (* Abstract type to represent UDP socket, with a phantom type parameter *)
  type 'a socket

  (* Creates a new UDP socket available for input and output operations and
   * receiving UDP packets on the given address. *)
  val mk_socket : addr -> [ `In | `Out ] socket

  (* Check for available operations on a socket. Sockets in one of the read or
   * write list are marked as ready for the corresponding operation when they
   * are. Internally calls [Unix.select]. *)
  val check_ops :
    ?read:([> `In ] socket list) ->
    ?write:([> `Out ] socket list) ->
    unit -> unit

  (* Immediate read. If data is available on the socket, [read] reads that data
   * and converts it into the correct data type (it may raise any exceptions
   * [Data.of_string] throws) and returns that data associated with the
   * sender's address. Otherwise, [read] returns [None]. *)
  val read : [> `In ] socket -> (input * addr) option

  (* Buffered write. Immediately converts the data to its string
   * representation, thus any exception from [Data.to_string] are immediately
   * raised. *)
  val write : [> `Out ] socket -> output -> addr -> unit

  (* Flush output buffer iff the socket was marked as ready for writing by a
   * previous call to [check_ops], and does nothing otherwise. Only one message
   * will be sent. *)
  val flush : [> `Out ] socket -> unit
end

module MakeTCP :
  functor (In : Bufferize) -> functor (Out : Show) ->
    TCP with type input = In.t and type output = Out.t

module MakeUDP :
  functor (In : Read) -> functor (Out : Show) ->
    UDP with type input = In.t and type output = Out.t
