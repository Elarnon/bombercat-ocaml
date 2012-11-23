open Common

let rec copy cin cout =
  match read cin with
  | Message m -> begin
      write cout (m ^ "\n"); flush cout; copy cin cout
    end
  | EOF -> ()
  | Nothing -> copy cin cout

let _ =
  if Array.length Sys.argv <> 2 then begin
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " ip[:port]");
    exit 2
  end else
    Unix.handle_unix_error (fun () ->
    let addr = parse_addr Sys.argv.(1) in
    let chan = connect (mk_socket ()) addr in
    Sys.set_signal
      Sys.sigusr1
      (Sys.Signal_handle (fun _ -> exit 0));
    match Unix.fork () with
    | -1 -> assert false (* TODO *)
    | 0 ->
        copy (in_chan Unix.stdin) chan;
        Unix.kill (Unix.getppid ()) Sys.sigusr1
    | p ->
        copy chan (out_chan Unix.stdout);
        Unix.kill p Sys.sigusr1
    ) ()
