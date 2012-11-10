type 'a chan =
  { sock : Unix.file_descr
  ; mutable can_read : bool
  ; rbuff : Buffer.t
  ; mutable can_write : bool
  ; wbuff : Buffer.t }

type read_result =
  | Message of string
  | EOF
  | Nothing
  
type addr = Unix.sockaddr
  
let chan_of_file =
  let (tbl : (Unix.file_descr, 'a chan) Hashtbl.t) = Hashtbl.create 17 in
  fun sock ->
    try Hashtbl.find tbl sock
    with Not_found -> begin
      let chan =
        { sock
        ; can_read = false
        ; rbuff = Buffer.create 16
        ; can_write = false
        ; wbuff = Buffer.create 16 } in 
      Hashtbl.add tbl sock chan;
      chan
    end
    
let in_chan = chan_of_file
and out_chan = chan_of_file

let file_of_chan { sock ; _ } = sock

let mk_socket () =
  chan_of_file Unix.(socket PF_INET SOCK_STREAM 0)

let bind ({ sock; _ } as chan) addr =
  let open Unix in
  setsockopt sock SO_REUSEADDR true;
  bind sock addr;
  set_nonblock sock;
  listen sock 3;
  chan
  
let connect ({ sock; _ } as chan) addr =
  Unix.connect sock addr; chan
  
(* Calls select on the underlying file_descr, then mark the chans with the
 * appropriate value.
 * As [Unix.select] counts server sockets as "ready to read" when they, in fact,
 * are "ready to accept", we merge the [accept] and [read] lists and use the
 * same field `can_read' for both. *)
let check_ops ?(accept=[]) ?(read=[]) ?(write=[]) () =
  let to_read =
    List.fold_left
      (fun r s -> s :: r)
      (List.map file_of_chan read)
      (List.map file_of_chan accept)
  and to_write = List.map file_of_chan write
  in let r, w, _e = Unix.select to_read to_write [] 1.0 in
  List.iter
    (fun s -> (chan_of_file s).can_read <- true)
    r;
  List.iter
    (fun s -> (chan_of_file s).can_write <- true)
    w;
  ()

let accept ({ sock; _ } as chan) =
  let cli_sock, _ = Unix.accept sock in
  chan.can_read <- false;
  chan_of_file cli_sock
  
let accept_if_possible ({ can_read ; _ } as chan) =
  if can_read then Some (accept chan)
  else None

let read ({ sock; rbuff; _ } as chan) =
  let sbuff = String.make 1024 '$' in
  let n = Unix.read sock sbuff 0 1024 in
  chan.can_read <- false;
  if n = 0 then EOF else
  try
    let i = String.index sbuff '\n' in
    let b = Buffer.contents rbuff in
    Buffer.reset rbuff;
    Buffer.add_substring rbuff sbuff (i + 1) (n - i - 1);
    Message (b ^ String.sub sbuff 0 i)
  with Not_found ->
    Buffer.add_substring rbuff sbuff 0 n;
    Nothing
    
let read_if_possible ({ can_read ; _ } as chan) =
  if can_read
  then read chan
  else Nothing
    
let write { wbuff; _ } m =
  Buffer.add_string wbuff m
  
let flush ({ sock; wbuff; _ } as chan) =
  let msg = Buffer.contents wbuff in
  Buffer.reset wbuff;
  let _n = Unix.write sock msg 0 (String.length msg) in
  chan.can_write <- false
  
let flush_if_possible ({ can_write; _ } as chan) =
  if can_write
  then flush chan

let get_addr { sock; _ } = Unix.getsockname sock

let get_peer_addr { sock; _ } = Unix.getpeername sock

let dns_resolve addr =
  let host = Unix.gethostbyname addr in
  let l = host.Unix.h_addr_list in
  if Array.length l = 0
  then raise Not_found
  else l.(Random.int (Array.length l))
  
let mk_addr ?(port=6669) addr =
  let unix_addr = dns_resolve addr in
  Unix.ADDR_INET (unix_addr, port)
  
let parse_addr s =
  let l = String.length s in
  let i =
    try String.index s ':' with Not_found -> l
  in
  let addr = String.sub s 0 i in
  let port =
    if i = l then None
    else try
      Some (int_of_string (String.sub s (i + 1) (l - i - 1)))
    with Failure "int_of_string" -> raise Not_found
  in mk_addr ?port addr

let string_of_addr addr =
  let open Unix in
  let ni = getnameinfo addr [] in
  ni.ni_hostname ^ ":" ^ ni.ni_service
