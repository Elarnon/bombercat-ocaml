open Unix

let clients = (sockaddr, player) Hashtbl.t

let run_server () =
  let addr = ADDR_INET (inet_addr_any, 12345) in
  let sock = Network.bind_socket addr in
  while true do
    match Network.next_message with
    | None -> ()
    | Some (str, addr) ->
        let msg = Bencode.of_string str in
        let cmd = Protocol.bdecode_client msg in
        let cli = Client.of_addr addr in
        Client.Command.push cli cmd
