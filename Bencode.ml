open Lwt
open Misc

module S = Lwt_stream

type t =
  | S of string
  | I of int 
  | L of t list
  | D of (string, t) Hashtbl.t

type bencoded = private string

exception Format_error of string

let list_iteri f l =
  let rec iteri n = function
    | [] -> ()
    | h::t -> f n h; iteri (n + 1) t
  in iteri 0 l

let nget_rev n s =
  let rec nget_ acc = function
    | 0 -> return acc
    | n ->
        S.next s >>= fun c ->
        nget_ (c :: acc) (n - 1)
  in nget_ [] n

let rec continue_bint s neg value endchar =
  S.next s >>= fun c ->
    if '0' <= c && c <= '9' then
        let ioc = int_of_char in
        continue_bint s neg (value * 10 + ioc c - ioc '0') endchar
    else if c = endchar then
        return (if neg then -value else value)
    else 
      fail (Format_error (Format.sprintf
        ("Unexpected character `%c` while reading a bencoded integer "
        ^^ "(expecting a digit or `%c`).") c endchar))
  
let negative_bint s endchar =
  S.next s >>= function
    | c when '1' <= c && c <= '9' -> continue_bint s true 0 endchar
    | c ->
        fail (Format_error (Format.sprintf
          ("Unexpected character `%c` after minus sign in bencoded integer "
          ^^ " (expecting a digit).") c))
  
let read_bzero s endchar =
  S.next s >>= fun c ->
    if c = endchar
    then return 0
    else
      fail (Format_error (Format.sprintf
        ("Unexpected character `%c` after reading a zero in bencode (`%c` "
        ^^ "expected).") c endchar))

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
          "Unexpected character `%c` while reading bencode integer (read_bint)."
          c))
    | None ->
        fail Lwt_stream.Empty

let rec read_blist s acc =
  S.peek s >>= fun c ->
    if c = Some 'e' then
      S.junk s >>= fun () ->
      return (List.rev acc)
    else
      read_bencode s >>= fun v ->
      read_blist s (v :: acc)
  
and read_bdict s hash last =
  S.peek s >>= function
    | Some 'e' -> S.junk s >>= fun () -> return hash
    | Some c ->
        read_bstring s >>= fun key ->
        (* TODO *)
        begin if Some key <= last then
          Lwt_log.error ("Illegal key order in bencoded dictionary. Reading " ^
            "anyway for compatibility.")
        else return () end >>
        read_bencode s >>= fun v ->
        Hashtbl.add hash key v;
        read_bdict s hash (Some key)
    | None ->
        fail Lwt_stream.Empty

and read_bstring s =
  read_bint s ':' >>= fun len ->
  nget_rev len s >>= fun l ->
  let s = String.make len ' ' in
  list_iteri (fun i c -> s.[len - i - 1] <- c) l;
  return s

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
        read_bdict s (Hashtbl.create 17) None >>= fun d -> return (D d)
    | Some c when '0' <= c && c <= '9' ->
        read_bstring s >>= fun s -> return (S s)
    | Some '\n' ->
        S.junk s >> read_bencode s (* TODO: remove *)
    | Some c ->
        fail (Format_error (Format.sprintf
          ("Unexpected initial character `%c` for bencoded value (expecting "
          ^^ "`i`, `l`, `d` or a digit).") c))
    | None ->
        fail Lwt_stream.Empty

let of_stream stream =
  try_lwt
    Lwt_stream.is_empty stream >>= fun empty ->
      if empty then return_none
      else Lwt_stream.parse stream read_bencode >|= fun x -> Some x
  with
  | Lwt_stream.Empty ->
      fail (Format_error
        "Unexpected end of stream while reading a bencoded value.")

let rec to_stream = function
  | S s -> S.of_string (string_of_int (String.length s) ^ ":" ^ s)
  | I i -> S.of_string ("i" ^ string_of_int i ^ "e")
  | L l -> (* [< "l"; concatMap to_stream l; "e" >] *)
      let contents = S.concat (S.map to_stream (S.of_list l)) in
      S.append (S.of_string "l") (S.append contents (S.of_string "e"))
  | D h ->
      let module Smap = Map.Make(String) in
      let smap = Hashtbl.fold Smap.add h Smap.empty in
      let beginning = Smap.fold (fun k v s ->
        S.append s (S.append (to_stream (S k)) (to_stream v)))
        smap (S.of_string "d") in
      S.append beginning (S.of_string "e")

let of_string s =
  of_stream (S.of_string s)

let to_string v =
  S.to_string (to_stream v)
