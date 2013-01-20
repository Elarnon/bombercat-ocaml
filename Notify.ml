type 'a t = ('a -> unit) Lwt_sequence.t

type mark = unit -> unit

let create = Lwt_sequence.create

let register seq f =
  let node = Lwt_sequence.add_r f seq in
  fun () -> Lwt_sequence.remove node

let unregister mark = mark ()

let notify seq arg =
  let wakeners = Lwt_sequence.fold_r (fun x l -> x :: l) seq [] in
  List.iter (fun f -> f arg) wakeners
