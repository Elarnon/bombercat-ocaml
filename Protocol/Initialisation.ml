open Lwt
  
open Network
  
open Misc
  
open Bencode
  
open Data
  
exception Error of string
  
(* TODO **)
let read_stream decoder catcher s =
  (Lwt_stream.is_empty s) >>=
    (function
     | true -> return_none
     | false ->
         Lwt.catch
           (fun () ->
              (Bencode.of_stream s) >>=
                (function
                 | Some x -> (decoder x) >|= (fun x -> Some x)
                 | None -> return_none))
           (fun exn -> Lwt.bind (catcher exn) (fun _ -> return None)))
  
type params =
  { p_game_time : int; p_bomb_time : int; p_bomb_dist : int;
    p_map_width : int; p_map_height : int; p_turn_time : int;
    p_start_delay : int; p_version : int
  }

let bencode_params p =
  D
    (map_of_list
       [ ("GameTime", (I p.p_game_time));
         ("BombExplosionTime", (I p.p_bomb_time));
         ("BombExplosionDistance", (I p.p_bomb_dist));
         ("MapWidth", (I p.p_map_width)); ("MapHeight", (I p.p_map_height));
         ("TurnTime", (I p.p_turn_time));
         ("StartDelay", (I p.p_start_delay));
         ("ProtocolVersion", (I p.p_version)) ])
  
let bdecode_params = let open Bencode
  in
    function
    | D tbl ->
        (try
           match gets
                   [ "GameTime"; "BombExplosionTime";
                     "BombExplosionDistance"; "MapWidth"; "MapHeight";
                     "TurnTime"; "StartDelay"; "ProtocolVersion" ]
                   tbl
           with
           | [ I gt; I bet; I bed; I mw; I mh; I tt; I sd; I pv ] ->
               return
                 {
                   p_game_time = gt;
                   p_bomb_time = bet;
                   p_bomb_dist = bed;
                   p_map_width = mw;
                   p_map_height = mh;
                   p_turn_time = tt;
                   p_start_delay = sd;
                   p_version = pv;
                 }
           | _ -> fail (Error "bdecode_params")
         with | Not_found -> fail (Error "bdecode_params"))
    | _ -> fail (Error "becode_params")
  
type client = | HELLO of string * int list

type server =
  | REJECTED of string
  | OK of string * Data.map * params
  | JOIN of string * string * char
  | START of Unix.tm * int
  | QUIT of string

let bencode_client = let open Bencode
  in
    function
    | HELLO (pseudo, versions) ->
        L [ S "HELLO"; S pseudo; L (List.map (fun i -> I i) versions) ]
  
let bdecode_client = let open Bencode
  in
    function
    | L ([ S "HELLO"; S pseudo; L bversions ]) ->
        let versions =
          Lwt_list.map_p
            (function
             | I i when i > 0 -> return i
             | _ -> fail (Error "bdecode_client"))
            bversions
        in versions >>= (fun v -> return @$ (HELLO (pseudo, v)))
    | _ -> fail (Error "bdecode_client")
  
let bencode_server = let open Bencode
  in
    function
    | REJECTED reason -> L [ S "REJECTED"; S reason ]
    | OK (id, map, params) ->
        L [ S "OK"; S id; S (Data.string_of_map map); bencode_params params ]
    | JOIN (pseudo, id, pos) ->
        L [ S "JOIN"; S pseudo; S id; S (String.make 1 pos) ]
    | START (date, nano) -> let open Unix
        in
          let {
                tm_sec = tm_sec;
                tm_min = tm_min;
                tm_hour = tm_hour;
                tm_mday = tm_mday;
                tm_mon = tm_mon;
                tm_year = tm_year;
                _
            } = date in
          let str =
            Format.sprintf "%04d-%02d-%02d %02d:%02d:%02d:%04d"
              (1900 + tm_year) tm_mon tm_mday tm_hour tm_min tm_sec nano
          in L [ S "START"; S str ]
    | QUIT ident -> L [ S "QUIT"; S ident ]
  
let bdecode_server = let open Bencode
  in
    function
    | L ([ S "REJECTED"; S reason ]) -> return @$ (REJECTED reason)
    | L ([ S "OK"; S id; S smap; bparams ]) ->
        (bdecode_params bparams) >>=
          (fun p -> return @$ (OK (id, (Data.map_of_string smap), p)))
    | L ([ S "JOIN"; S pseudo; S id; S pos ]) -> (* when *)
        (* String.length pos = 1 && pos.[0] >= 'A' && pos.[0] <= 'Z' -> *)
        return @$ (JOIN (pseudo, id, pos.[0]))
    | L ([ S "START"; S strdate ]) -> let open Unix
        in
          (try
             return @$
               (Scanf.sscanf strdate "%04d-%02d-%02d %02d:%02d:%02d:%04d"
                  (fun tm_year tm_mon tm_mday tm_hour tm_min tm_sec nano ->
                     let raw =
                       {
                         tm_sec = tm_sec;
                         tm_min = tm_min;
                         tm_hour = tm_hour;
                         tm_mday = tm_mday;
                         tm_mon = tm_mon;
                         tm_year = tm_year - 1900;
                         tm_yday = 0;
                         tm_wday = 0;
                         tm_isdst = false;
                       }
                     in START ((snd (Unix.mktime raw)), nano)))
           with
           | Scanf.Scan_failure _ -> fail (Error "bdecode_server start date"))
    | L ([ S "QUIT"; S ident ]) -> return @$ (QUIT ident)
    | _ -> fail (Error "bdecode_server")
  
module Server =
  struct
    type input = client
    
    type output = server
    
    let input_of_stream =
      read_stream bdecode_client
        (function
         | Bencode.Format_error e ->
             Lwt_log.error
               ("Bencode format error while decoding messages from " ^
                  ("Protocol.Init client: " ^ e))
         | Error where -> Lwt_log.error where
         | exn -> fail exn)
      
    let stream_of_output v = Bencode.to_stream (bencode_server v)
      
  end
  
module Client =
  struct
    type input = server
    
    type output = client
    
    let input_of_stream =
      read_stream bdecode_server
        (function
         | Bencode.Format_error e ->
             Lwt_log.error
               ("Bencode format error while decoding messags from " ^
                  ("Protocol.Init server: " ^ e))
         | Error where -> Lwt_log.error where
         | exn -> fail exn)
      
    let stream_of_output v = Bencode.to_stream (bencode_client v)
      
  end
  

