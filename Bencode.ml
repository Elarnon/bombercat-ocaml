type t =
  | S of string
  | I of int
  | L of t list
  | D of (t, t) Hashtbl.t

type bencode = t

type 'a result =
  | Cont of int * string * (string * int -> char -> 'a result)
  | Val of 'a * string
  
type bencoded = string

exception Format_error

let ioc = int_of_char

let get (s, init) i k =
  if i - init >= String.length s then
    Cont (String.length s + init, "", k)
  else k (s, init) s.[i - init]

let string_of_list len lst =
  let s = String.make len ' ' in
  List.iteri (fun i c -> s.[i] <- c) lst;
  s

let substring s i len k =
  let rec sub acc i = function
    | 0 -> k s (string_of_list len (List.rev acc))
    | l -> get s i (fun s c ->
        sub (c :: acc) (i + 1) (l - 1))
  in sub [] i len

let rec continue_bint s i neg value endchar k =
  get s i (fun s -> function
    | c when '0' <= c && c <= '9' ->
        continue_bint s (i + 1) neg (value * 10 + ioc c - ioc '0') endchar k
    | c when c = endchar -> k s ( I (if neg then -value else value), i + 1 )
    | _ -> raise ( Format_error )
  )
  
let negative_bint s i endchar k =
  get s i (fun s -> function
    | c when '1' <= c && c <= '9' -> continue_bint s i true 0 endchar k
    | _ -> raise ( Format_error )
  )
  
let read_bzero s i endchar k =
  get s i (fun s c ->
    if c = endchar
    then k s ( I 0, i + 1 )
    else raise ( Format_error )
  )
  
let rec read_bint s i endchar k =
  get s i (fun s -> function
    | '-' -> negative_bint s (i + 1) endchar k
    | '0' -> read_bzero s (i + 1) endchar k
    | c when '1' <= c && c <= '9' -> continue_bint s i false 0 endchar k
    | _ -> raise ( Format_error )
  )
  
and read_blist s i acc k =
  get s i (fun s -> function
    | 'e' -> k s ( L (List.rev acc), i + 1 )
    | c ->
        process s i (fun s (next, i') ->
          read_blist s i' (next :: acc) k)
  )
  
and read_bdict s i hash k =
  get s i (fun s -> function
    | 'e' -> k s ( D hash, i + 1)
    | c ->
        process s i (fun s (key, i') ->
          process s i' (fun s (v, i'') ->
            Hashtbl.add hash key v;
            read_bdict s i'' hash k))
  )

and read_bstring s i k =
  read_bint s i ':' (fun s -> function
    | (I len, i') ->
      substring s i' len (fun s sub -> k s ( S sub, i' + len ))
    | _ -> assert false
  )
  
and process s i k =
  get s i (fun s -> start s i k)

and start s i k = function
    | 'i' -> read_bint s (i + 1) 'e' k
    | 'l' -> read_blist s (i + 1) [] k
    | 'd' -> read_bdict s (i + 1) (Hashtbl.create 17) k
    | c when '0' <= c && c <= '9' -> read_bstring s i k
    | _ -> raise ( Format_error )
  
let empty_fun = fun s -> start s 0
    (fun (s, init) (x, i) ->
      let len = String.length s in
      Val (x, String.sub s (init + i) (len - init - i)))

let decode s =
  match get (s, 0) 0 empty_fun with
  | Val (x, "") -> x
  | _ -> raise Format_error

let rec encode = function
  | S s ->
      string_of_int (String.length s) ^ ":" ^ s
  | I i ->
      "i" ^ string_of_int i ^ "e"
  | L l ->
      "l" ^ List.fold_left (fun s e -> s ^ encode e) "" l ^ "e"
  | D h ->
      "d" ^ Hashtbl.fold (fun k v s -> s ^ encode k ^ encode v) h "" ^ "e"
      
let of_string = decode

let to_string = encode

module Bufferize : Network.Bufferize with type t = t = struct
  type t = bencode
  type buffer = bencode result ref
  let empty_buffer () =
    ref ( Cont (0, "", empty_fun) )
  let add_buffer buffer add =
    match !buffer with
    | Cont (i, s, k) ->
        buffer := get (s ^ add, i) i k
    | Val (x, s) -> buffer := Val (x, s ^ add)
  let rec consume_buffer buffer =
    match !buffer with
    | Cont (_, "", _) -> None
    | Cont (i, s, k) ->
        buffer := get (s, i) i k;
        consume_buffer buffer
    | Val (x, s) -> begin
      buffer := Cont (0, s, empty_fun);
      Some x
    end
  let to_string = encode
end

module Show : Network.Show with type t = t = struct
  type t = bencode

  let show = to_string
end

module Read : Network.Read with type t = t = struct
  type t = bencode

  let read = of_string
end

module TCP = Network.MakeTCP(Bufferize)(Show)

module UDP = Network.MakeUDP(Read)(Show)

module type Wrapper = sig
  type t
  val encode : t -> bencode
  val decode : bencode -> t
end

module WrapBufferize(W : Wrapper) = struct
  type t = W.t
  type buffer = Bufferize.buffer
  let empty_buffer = Bufferize.empty_buffer
  let add_buffer = Bufferize.add_buffer
  let consume_buffer buffer =
    match Bufferize.consume_buffer buffer with
    | None -> None
    | Some b -> Some (W.decode b)
end

module WrapShow(W : Wrapper) = struct
  type t = W.t
  let show v = Show.show (W.encode v)
end

module WrapRead(W : Wrapper) = struct
  type t = W.t
  let read s = W.decode (Read.read s)
end
