open Misc

(* Modules to make our monadic code work both in Lwt and Identity monads *)

module type CharStream = sig
  module Monad : sig
    type 'a t

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    val return : 'a -> 'a t

    val fail : exn -> 'b t

    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  end

  type 'a t

  exception Empty

  val of_string : string -> char t

  val next : char t -> char Monad.t

  val peek : char t -> char option Monad.t

  val junk : char t -> unit Monad.t

  val is_empty : char t -> bool Monad.t

  val parse : char t -> (char t -> 'a Monad.t) -> 'a Monad.t
end

module StringCharStream = struct
  module Monad = struct
    type 'a t = 'a

    let (>>=) x f = f x

    let return x = x

    let fail x = raise x

    let catch f err =
      try f ()
      with e -> err e
  end

  type 'a t = string * int ref

  exception Empty

  open Monad

  let of_string str = (str, ref 0)

  let get (str, pos) = return @$
    try str.[!pos]
    with Invalid_argument _ -> fail Empty

  let peek (str, pos) = return @$
    try Some str.[!pos]
    with Invalid_argument _ -> None

  let junk (_, pos) =
    pos := !pos + 1

  let next s =
    get s >>= fun res ->
    junk s >>= fun () ->
    return res
      
  let is_empty (s, pos) = return @$ !pos >= String.length s

  let parse (s, pos) f =
    let rem = !pos in
    catch (fun () -> f (s, pos)) (fun e -> pos := rem; fail e)
end

module LwtCharStream = struct
  module Monad = Lwt

  include Lwt_stream
end

(* Real stuff *)

type t =
  | S of string
  | I of int 
  | L of t list
  | D of t Smap.t

exception Format_error of string

module Make(S : CharStream) = struct

  open S.Monad

  (* Gets [n] characters from [s] and returns then as a list, in reverse order
   *)
  let nget_rev n s =
    let rec nget_ acc = function
      | 0 -> return acc
      | n ->
          S.next s >>= fun c ->
          nget_ (c :: acc) (n - 1)
    in nget_ [] n

  (* Finishes reading a bencoded integer from [s]. [neg] indicates whether this
   * integer must be returned as a negative value or not, [value] keeps the
   * currently read value (ie, the value read with the previous chars) and
   * [endchar], which should be either ':' or 'e', is the character that marks
   * the end of the integer. *)
  let rec continue_bint s neg value endchar =
    S.next s >>= fun c ->
      if '0' <= c && c <= '9' then
          let ioc = int_of_char in
          continue_bint s neg (value * 10 + ioc c - ioc '0') endchar
      else if c = endchar then
          return (if neg then -value else value)
      else 
        fail (Format_error (Format.sprintf
          ("Unexpected character `%c` while reading an integer "
          ^^ "(expecting a digit or `%c`).") c endchar))
    
  (* Reads a negative integer, knowing that the '-' sign has already been
   * consumed, from [s]. [endchar] is the character marking the end of the
   * integer. *)
  let negative_bint s endchar =
    S.next s >>= function
      | c when '1' <= c && c <= '9' -> continue_bint s true 0 endchar
      | c ->
          fail (Format_error (Format.sprintf
            ("Unexpected character `%c` after minus sign in integer "
            ^^ " (expecting a digit).") c))
    
  (* Reads a zero, knowing that the '0' character has already been consumed,
   * from [s]. [endchar] is the character marking the end of the integer, which
   * must be the next character in the stream. *)
  let read_bzero s endchar =
    S.next s >>= fun c ->
      if c = endchar
      then return 0
      else
        fail (Format_error (Format.sprintf
          ("Unexpected character `%c` after reading a null integer "
          ^^ "(expecting `%c`).") c endchar))

  (* Reads an integer from [s], with [endchar] as an end delimiter. *)
  let read_bint s endchar =
    S.peek s >>= function
      | Some '-' ->
          S.junk s >>= fun () ->
          negative_bint s endchar
      | Some '0' ->
          S.junk s >>= fun () ->
          read_bzero s endchar
      | Some c when '1' <= c && c <= '9' -> continue_bint s false 0 endchar
      | Some c ->
          fail (Format_error (Format.sprintf
            ("Unexpected character `%c` while reading an integer "
            ^^ " (expecting `-` or a digit).") c))
      | None ->
          fail S.Empty

  (* Reads a list of values from [s], accumulating them into [acc], knowing that
   * the initial 'l' has already been consumed. *)
  let rec read_blist s acc =
    S.peek s >>= fun c ->
      if c = Some 'e' then
        S.junk s >>= fun () ->
        return (List.rev acc)
      else
        read_bencode s >>= fun v ->
        read_blist s (v :: acc)
    
  (* Reads a dictionary of values from [s], accumulating them in [map], and
   * using [last] to check that the keys are in order. The initial 'd' has
   * already been consumed. *)
  and read_bdict s map last =
    S.peek s >>= function
      | Some 'e' -> S.junk s >>= fun () -> return map
      | Some _ ->
          read_bstring s >>= fun key ->
          if Some key <= last then
            fail (Format_error "Illegal key order in dictionary.")
          else 
            read_bencode s >>= fun v ->
            read_bdict s (Smap.add key v map) (Some key)
      | None ->
          fail S.Empty

  (* Reads a string from [s], without prerequisite *)
  and read_bstring s =
    read_bint s ':' >>= fun len ->
    nget_rev len s >>= fun l ->
    let s = String.make len ' ' in
    list_iteri (fun i c -> s.[len - i - 1] <- c) l;
    return s

  (* Deterministically calls, by lookup of the first char in [s], the correct
   * data reader. *)
  and read_bencode s =
    S.peek s >>= function
      | Some 'i' ->
          S.junk s >>= fun () ->
          read_bint s 'e' >>= fun i -> return (I i)
      | Some 'l' ->
          S.junk s >>= fun () ->
          read_blist s [] >>= fun l -> return (L l)
      | Some 'd' ->
          S.junk s >>= fun () ->
          read_bdict s Smap.empty None >>= fun d -> return @$ D d
      | Some c when '0' <= c && c <= '9' ->
          read_bstring s >>= fun s -> return (S s)
      | Some '\n' ->
          S.junk s >>= fun () -> read_bencode s (* TODO: remove *)
      | Some c ->
          fail (Format_error (Format.sprintf
            ("Unexpected initial character `%c` (expecting "
            ^^ "`i`, `l`, `d` or a digit).") c))
      | None ->
          fail S.Empty

  (* Converts a stream of characters into a bencoded value *)
  let of_stream stream =
    catch (fun () ->
      S.is_empty stream >>= fun empty ->
        if empty then return None
        else S.parse stream read_bencode >>= fun x -> return @$ Some x)
      (function
         | S.Empty ->
             fail (Format_error
                  "Unexpected end of connection (probably partial value).")
         | e -> fail e)

  (* Converts a bencoded value into a string... *)
  let rec to_string = function
    | S s -> string_of_int (String.length s) ^ ":" ^ s
    | I i -> "i" ^ string_of_int i ^ "e"
    | L l -> (* [< "l"; concatMap to_stream l; "e" >] *)
        let contents = String.concat "" (List.map to_string l) in
        "l" ^ contents ^ "e"
    | D m ->
        let beginning = Smap.fold (fun k v s ->
          s ^ to_string (S k) ^ to_string v) m "d" in
        beginning ^ "e"

  (* ... in order to convert it into a stream. *)
  let to_stream v =
    S.of_string (to_string v)

end

(* Creates the modules with "string" streams and Lwt_stream.t *)
module BString = Make(StringCharStream)

module BStream = Make(LwtCharStream)

(* Use them to define the conversion functions *)
let to_string b = fst @$ BString.to_stream b

let to_stream = BStream.to_stream

let of_string s = BString.of_stream (StringCharStream.of_string s)

let of_stream = BStream.of_stream

let most_to_string len vs =
  let rec travel str n = function
    | [] -> "l" ^ str ^ "e", n
    | elt :: rest ->
        let bstr = to_string elt in
        let nstr = bstr ^ str in
        if String.length nstr > len - 2 then
          ("l" ^ str ^ "e", n)
        else
          travel nstr (n + 1) rest
  in travel "" 0 vs

let all_to_strings len lst =
  let rec trec acc = function
    | [] -> List.rev acc
    | lst ->
        let packet, nb = most_to_string len lst in
        if nb = 0 then failwith "all_to_strings" else
        trec (packet :: acc) (discard nb lst)
  in trec [] lst
