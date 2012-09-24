open Common

let msgs = Queue.create ()

let addr cli =
  string_of_addr (get_peer_addr cli)
  
let id cli =
  let vowels = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'] in
  let rec mk_alphabet acc = function
    | 'z' -> 'z' :: acc
    | l -> l :: mk_alphabet acc (char_of_int (int_of_char l + 1))
  in let alphabet = mk_alphabet [] 'a' in
  let alen = List.length alphabet
  and vlen = List.length vowels in
  let a = addr cli in
  let l = String.length a in
  let tab = Array.init ((l + 3) / 4)
    (fun i ->
      let low = i * 4
      and high = min (i * 4 + 3) (l - 1)
      and v = ref 0 in
      for j = low to high do
        v := !v lor (int_of_char a.[j] lsl (8 * (high - j)))
      done;
      !v) in
  let s = Random.State.make tab in
  let rec mk acc last_vowel = function
    | 20 -> acc
    | n ->
        let e = Random.State.int s 110 in
        if n > 4 && n * 10 > e then acc else
        if last_vowel > 1 then
          let r = Random.State.int s vlen in
          mk (List.nth vowels r :: acc) 0 (n + 1)
        else
          let r = Random.State.int s alen in
          mk (List.nth alphabet r :: acc) (last_vowel + 1) (n + 1)
  in let name = mk [] 0 0 in
  let s = String.make (List.length name) ' ' in
  let _ = List.fold_left
    (fun i l -> s.[i] <- l; i + 1) 0 name in
  s

let treat_all_messages clients =
  List.fold_left
    (fun ok cli ->
      match read_if_possible cli with
      | Message m ->
          Queue.add ("< " ^ id cli ^ "> " ^ m) msgs;
          cli :: ok
      | Nothing -> cli :: ok
      | EOF ->
          Queue.add (id cli ^ " <" ^ addr cli ^ "> has quit") msgs;
          ok)
    [] clients
    
let broadcast m clients =
  print_endline m;
  List.iter (fun cli -> write cli (m ^ "\n")) clients
    
let send_all_messages clients =
  while not (Queue.is_empty msgs) do
    let m = Queue.pop msgs in
    broadcast m clients
  done;
  List.iter flush_if_possible clients
    
let rec server_step serv clients =
  check_ops ~accept:[serv] ~read:clients ~write:clients ();
  match accept_if_possible serv with
  | Some cli -> 
      Queue.add (id cli ^ " <" ^ addr cli ^ "> has connected") msgs;
      server_step serv (cli :: clients)
  | None ->
      let new_clients = treat_all_messages clients in
      send_all_messages new_clients;
      server_step serv new_clients
let _ =
  if Array.length Sys.argv <> 2 then begin
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " ip[:port]");
    exit 2
  end else
    Unix.handle_unix_error (fun () ->
    let addr = parse_addr Sys.argv.(1) in
    let chan = bind (mk_socket ()) addr in
    server_step chan []) ()
