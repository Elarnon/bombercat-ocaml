open Lwt

module Smap = Map.Make(String)

let (@$) f x = f x

let merge ?(quit=false) streams =
  let with_stream s =
    Lwt_stream.peek s >>= fun x -> return (x, s) in
  let ended, running = ref [], ref (List.map with_stream streams) in
  let rec step () =
    match !ended with
    | (x, s) :: ended' ->
        Lwt_stream.junk s >>= fun () ->
        ended := ended';
        running := with_stream s :: !running;
        return (Some x)
    | [] ->
        if !running = [] then return None
        else
          Lwt.nchoose_split !running >>= fun (ended', running') ->
          let end_now = ref false in
          let ended'' = List.filter
                          (fun x ->
                             if fst x = None then begin
                               end_now := quit; false
                             end else true)
                          ended' in
          if !end_now then
            return None
          else begin
            let ended_ = List.map
              (function | (None, _) -> assert false
                        | (Some x, s) -> (x, s)) ended'' in
            ended := ended_; running := running'; step ()
          end
  in Lwt_stream.from step

let wrap_eintr f =
  let rec loop () =
    try_lwt
      f ()
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in loop

let map_of_list l =
  List.fold_left (fun m (k, v) -> Smap.add k v m) Smap.empty l

let gets lst map = List.map (fun k -> Smap.find k map) lst
