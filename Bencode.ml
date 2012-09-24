type t =
  | String of string
  | Integer of int
  | List of t list
  | Dict of (t, t) Hashtbl.t
  
type bencoded = string

exception Format_error of string * int

let ioc = int_of_char

let get s i =
  try
    s.[i]
  with Invalid_argument _ -> raise ( Format_error (s, i) )

let rec continue_bint s i neg value endchar =
  match get s i with
  | c when '0' <= c && c <= '9' ->
      continue_bint s (i + 1) neg (value * 10 + ioc c - ioc '0') endchar
  | c when c = endchar -> ( Integer (if neg then -value else value), i + 1 )
  | _ -> raise ( Format_error (s, i) )
  
let negative_bint s i endchar =
  match get s i with
  | c when '1' <= c && c <= '9' -> continue_bint s i true 0 endchar
  | _ -> raise ( Format_error (s, i) )
  
let read_bzero s i endchar =
  if get s i = endchar
  then ( Integer 0, i + 1 )
  else raise ( Format_error (s, i) )
  
let rec read_bint s i endchar =
  match get s i with
  | '-' -> negative_bint s (i + 1) endchar
  | '0' -> read_bzero s (i + 1) endchar
  | c when '1' <= c && c <= '9' -> continue_bint s i false 0 endchar
  | _ -> raise ( Format_error (s, i) )
  
and read_blist s i acc =
  match get s i with
  | 'e' -> ( List (List.rev acc), i + 1 )
  | c ->
      let (next, i') = process s i in
      read_blist s i' (next :: acc)
  
and read_bdict s i hash =
  match get s i with
  | 'e' -> ( Dict hash, i + 1)
  | c ->
      let (k, i') = process s i in
      let (v, i'') = process s i' in
      Hashtbl.add hash k v;
      read_bdict s i'' hash

and read_bstring s i =
  match read_bint s i ':' with
  | (Integer len, i') -> begin
      try
        let sub = String.sub s i' len in
        ( String sub, i' + len )
      with Invalid_argument _ -> raise ( Format_error (s, String.length s) )
    end
  | _ -> assert false
  
and process s i =
  match get s i with
  | 'i' -> read_bint s (i + 1) 'e'
  | 'l' -> read_blist s (i + 1) []
  | 'd' -> read_bdict s (i + 1) (Hashtbl.create 17)
  | c when '0' <= c && c <= '9' -> read_bstring s i
  | _ -> raise ( Format_error (s, i) )
  
let decode s = fst (process s 0)

let rec encode = function
  | String s ->
      string_of_int (String.length s) ^ ":" ^ s
  | Integer i ->
      "i" ^ string_of_int i ^ "e"
  | List l ->
      "l" ^ List.fold_left (fun s e -> s ^ encode e) "" l ^ "e"
  | Dict h ->
      "d" ^ Hashtbl.fold (fun k v s -> s ^ encode k ^ encode v) h "" ^ "e"
      
let of_string = decode
