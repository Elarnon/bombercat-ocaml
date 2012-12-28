open Lwt

let some x = return (Some x)

let none = return None

let (>>>=) t f =
  t >>= function
    | Some v -> f v
    | None -> none

