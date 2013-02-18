open Misc

type 'a t =
  { table : (int, 'a) Hashtbl.t
  ; queue : 'a Queue.t
  ; window : int option
  ; max_size : int option
  ; mutable last_queued : int
  }

exception Empty

let create ?window ?max_size size =
  { table = Hashtbl.create size
  ; queue = Queue.create ()
  ; window
  ; max_size
  ; last_queued = -1
  }

let flush t =
  try while t.max_size = None || Some (Queue.length t.queue) < t.max_size do
    let to_queue = hashtbl_take t.table (t.last_queued + 1) in
    Queue.add to_queue t.queue;
    t.last_queued <- t.last_queued + 1
  done with Not_found -> ()

let add t id v =
  match t.window with
    | Some v when id > t.last_queued + v -> false
    | _ -> (* Otherwise, already queued -- too late for editing *)
        if id > t.last_queued then begin
          Hashtbl.replace t.table id v;
          flush t
        end;
        true

let interrupt ?notify t =
  flush t;
  Hashtbl.clear t.table;
  begin match notify with
    | None -> ()
    | Some f -> Queue.add (f t.last_queued) t.queue
  end;
  t.last_queued <- -1

let qwrap1 f ({ queue; _ } as t) =
  flush t;
  try f queue with Queue.Empty -> raise Empty

let peek t = qwrap1 Queue.peek t

let take t = qwrap1 Queue.take t

let junk t = qwrap1 queue_junk t

let last_id { last_queued; _ } = last_queued

let is_full { queue; max_size; _ } =
  Some (Queue.length queue) < max_size
