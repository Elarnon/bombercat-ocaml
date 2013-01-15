open Lwt

let some x = return (Some x)

let none = return None

let (>>>=) t f =
  t >>= function
    | Some v -> f v
    | None -> none


let (@$) f x = f x

let merge : 'a Lwt_stream.t list -> 'a Lwt_stream.t = function streams ->
  let with_stream s =
    Lwt_stream.peek s >>= fun x -> return (x, s) in
  let ended, running = ref [], ref (List.map with_stream streams) in
  let rec step () =
    match !ended with
    | (x, s) :: ended' ->
        Lwt_stream.junk s;
        ended := ended';
        running := with_stream s :: !running;
        return (Some x)
    | [] ->
        if !running = [] then return None
        else
          Lwt.nchoose_split !running >>= fun (ended', running') ->
          let ended'' = List.filter (fun x -> fst x <> None) ended' in
          let ended_ = List.map
            (function | (None, _) -> assert false
                      | (Some x, s) -> (x, s)) ended'' in
          ended := ended_; running := running'; step ()
  in Lwt_stream.from step
