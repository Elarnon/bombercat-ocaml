open Lwt
open Misc

module S = Lwt_stream

type t =
  | S of string
  | I of int 
  | L of t list
  | D of (string, t) Hashtbl.t

type bencoded = private string

let list_iteri f l =
  let rec iteri n = function
    | [] -> ()
    | h::t -> f n h; iteri (n + 1) t
  in iteri 0 l

let nget_rev n s =
  let rec nget_ acc = function
    | 0 -> some acc
    | n ->
        S.get s >>>= fun c ->
        nget_ (c :: acc) (n - 1)
  in nget_ [] n

let rec continue_bint s neg value endchar =
  S.get s >>>= fun c ->
    if '0' <= c && c <= '9' then
        let ioc = int_of_char in
        continue_bint s neg (value * 10 + ioc c - ioc '0') endchar
    else if c = endchar then
        some (if neg then -value else value)
    else none
  
let negative_bint s endchar =
  S.get s >>>= function
    | c when '1' <= c && c <= '9' -> continue_bint s true 0 endchar
    | _ -> none
  
let read_bzero s endchar =
  S.get s >>>= fun c ->
    if c = endchar
    then some 0
    else none

let read_bint s endchar =
  S.peek s >>>= function
    | '-' ->
        S.junk s >>= fun () ->
        negative_bint s endchar
    | '0' ->
        S.junk s >>= fun () ->
        read_bzero s endchar
    | c when '1' <= c && c <= '9' -> continue_bint s false 0 endchar
    | _ -> none

let rec read_blist s acc =
  S.peek s >>>= fun c ->
    if c = 'e' then
      S.junk s >>= fun () ->
      some (List.rev acc)
    else
      of_stream s >>>= fun v ->
      read_blist s (v :: acc)
  
and read_bdict s hash last =
  S.peek s >>>= function
    | 'e' -> S.junk s >>= fun () -> some hash
    | c ->
        read_bstring s >>>= fun key ->
        (* if Some key <= last
          then fail (Format_error "read_bdict")
          else *)
            of_stream s >>>= fun v ->
            Hashtbl.add hash key v;
            read_bdict s hash (Some key)

and read_bstring s =
  read_bint s ':' >>>= fun len ->
  nget_rev len s >>>= fun l ->
  let s = String.make len ' ' in
  list_iteri (fun i c -> s.[len - i - 1] <- c) l;
  some s

and of_stream s =
  S.peek s >>>= function
    | 'i' ->
        S.junk s >>= fun () ->
        read_bint s 'e' >>>= fun i -> some (I i)
    | 'l' ->
        S.junk s >>= fun () ->
        read_blist s [] >>>= fun l -> some (L l)
    | 'd' ->
        S.junk s >>= fun () ->
        read_bdict s (Hashtbl.create 17) None >>>= fun d -> some (D d)
    | c when '0' <= c && c <= '9' ->
        read_bstring s >>>= fun s -> some (S s)
    | '\n' -> S.junk s >>= fun () -> of_stream s (* TODO: remove *)
    | _ -> none
    (* | _ -> S.junk s >>= fun () -> of_stream s *)

let rec to_stream = function
  | S s -> S.of_string (string_of_int (String.length s) ^ ":" ^ s)
  | I i -> S.of_string ("i" ^ string_of_int i ^ "e")
  | L l -> (* [< "l"; concatMap to_stream l; "e" >] *)
      let contents = S.concat (S.map to_stream (S.of_list l)) in
      S.append (S.of_string "l") (S.append contents (S.of_string "e"))
  | D h ->
      let beginning = Hashtbl.fold (fun k v s ->
        S.append s (S.append (to_stream (S k)) (to_stream v)))
        h (S.of_string "d") in
      S.append beginning (S.of_string "e")

let of_string s =
  of_stream (S.of_string s)

let to_string v =
  S.to_string (to_stream v)
