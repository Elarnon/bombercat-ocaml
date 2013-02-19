open Lwt
open Network
open Misc

exception Error of string

                     (* TODO **)
let read_stream decoder catcher s =
  Lwt_stream.is_empty s >>= function
    | true -> return_none
    | false ->
        try_lwt
          Bencode.of_stream s >>= function
            | Some x -> decoder x >|= fun x -> Some x
            | None -> return_none
        with
          | exn -> catcher exn >> return None

type client =
  | ADD of addr * string * int
  | UPDATE of int * int
  | DELETE of int
  | LIST

type game =
  { game_id : int
  ; game_addr : Network.addr
  ; game_name : string
  ; game_nb_players : int
  ; game_max_players : int
  }

type server =
  | ADDED of int
  | GAMES of game list

let encode_client = function
  | ADD (addr, name, nb_players) ->
      let (ip, port) = raw_addr addr in
      Bencode.(L [
        S "ADD";
        S ip;
        I port;
        S name;
        I nb_players
      ])
  | UPDATE (game_id, nb_players) ->
      Bencode.(L [
        S "UPDATE";
        I game_id;
        I nb_players
      ])
  | DELETE game_id ->
      Bencode.(L [
        S "DELETE";
        I game_id
      ])
  | LIST -> Bencode.S "LIST"

let decode_client = let open Bencode in function
  | L ( S "ADD" :: S ip :: I port :: S name :: I nb_players :: _ ) ->
      lwt addr =
        try_lwt mk_addr ~port ip
        with Not_found -> fail (Error
          ("Ill-formed address received in ADD message from a Protocol.Meta
          client."))
      in if nb_players < 0
      then
        fail (Error 
          ("Unexpected negative number of players while reading ADD message "
          ^ "from a Protocol.Meta client."))
      else return (ADD (addr, name, nb_players))
  | L ( S "UPDATE" :: I game_id :: I nb_players :: _ ) ->
      if nb_players < 0
      then
        fail (Error
          ("Unexpected negative number of players while reading ADD message "
          ^ "from a Protocol.Meta client."))
      else return (UPDATE (game_id, nb_players))
  | L ( S "DELETE" :: I game_id :: _ ) ->
      return (DELETE game_id)
  | S "LIST" -> return LIST
  | _v ->
      fail (Error "Invalid message from Protocol.Meta client.")

let bencode_game g =
  let open Bencode in
  let (ip, port) = raw_addr g.game_addr in
  D (map_of_list
    [ ("Id", I g.game_id)
    ; ("Ip", S ip)
    ; ("Port", I port)
    ; ("Name", S g.game_name)
    ; ("Players", I g.game_nb_players)
    ; ("MaxPlayers", I g.game_max_players)
    ])

let bdecode_game = let open Bencode in function
  | D m -> begin
    try
      match gets ["Id"; "Ip"; "Port"; "Name"; "Players"; "MaxPlayers"] m with
      | [I id; S ip; I port; S name; I players; I max_players] when
        players >= 0 && max_players >= players && max_players > 0 ->
          lwt game_addr = mk_addr ip ~port in
          return
            { game_id = id
            ; game_addr
            ; game_name = name
            ; game_nb_players = players
            ; game_max_players = max_players
            }
      | _ ->
          fail (Error "Ill-typed game parameters dictionary.")
    with Not_found ->
      fail (Error "Missing key in game parameters dictionary.")
  end
  | _ -> fail (Error ("Unexpected non-dictionary value passed as game "
        ^ "parameters."))

let encode_server = let open Bencode in function
  | ADDED id -> L [ S "ADDED"; I id ]
  | GAMES games ->
      L (S "GAMES" :: List.map bencode_game games)

let decode_server = let open Bencode in function
  | L [ S "ADDED"; I id ] -> return (ADDED id)
  | L (S "GAMES" :: q) ->
      Lwt_list.map_p bdecode_game q >>= fun games ->
      return (GAMES games)
  | _ -> fail (Error "Invalid message from Protocol.Meta server.")

module Server = struct
  type input = client
  type output = server

  let input_of_stream s =
    read_stream
      decode_client
      (function
         | Bencode.Format_error e ->
             Lwt_log.error
              ("Bencode format error while decoding messages from " ^
              "Protocol.Meta client: " ^ e)
         | Error where ->
             Lwt_log.error where
         | exn -> fail exn)
      s

  let stream_of_output v = Bencode.to_stream (encode_server v)
end

module Client = struct
  type input = server
  type output = client

  let input_of_stream s =
    read_stream
      decode_server
      (function
         | Bencode.Format_error e ->
             Lwt_log.error
               ("Bencode format error while decoding messages from " ^
               "Protocol.Meta server: " ^ e)
         | Error where ->
             Lwt_log.error where
         | exn -> fail exn)
      s

  let stream_of_output v = Bencode.to_stream (encode_client v)
end
