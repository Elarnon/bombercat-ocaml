type ('i, 'o) coroutine = { coroutine : 'i -> 'o * ('i, 'o) coroutine }

let runC { coroutine } = coroutine

let rec id = { coroutine = fun i -> (i, id) }

let rec ( <<< ) cof cog =
  { coroutine = fun i ->
    let x, cog' = runC cog i in
    let y, cof' = runC cof x in
    ( y, cof' <<< cog' ) }

let ( >>> ) cog cof = cof <<< cog

let rec arr f = { coroutine = fun i -> (f i, arr f) }

let rec first co =
  { coroutine = function (i, r) ->
    let l, co' = runC co i in
    ( (l, r), first co' ) }
  
let rec second co =
  { coroutine = function (l, i) ->
    let r, co' = runC co i in
    ( (l, r), second co' ) }
  
let rec ( *** ) cof cog =
  { coroutine = function (f_in, g_in) ->
    let f_out, cof' = runC cof f_in
    and g_out, cog' = runC cog g_in in
    ( (f_out, g_out), cof' *** cog' ) }
  
let rec ( &&& ) cof cog =
  { coroutine = fun i ->
    let f_out, cof' = runC cof i
    and g_out, cog' = runC cog i in
    ( (f_out, g_out), cof' &&& cog' ) }
  
let app =
  let rec mk =
    { coroutine = function (co, i) ->
      let o, co' = runC co i in
      ( o, mk ) }
  in mk
  
let rec loop co y =
  { coroutine = fun i ->
    let (x, y'), co' = runC co (i, y)
    in (x, loop co' y') }
  
let lazy_update f =
  let co = arr (function (i, mem) ->
    match mem with
    | Some (lasti, lasto) when i = lasti -> (lasto, mem)
    | _ ->
        let o = f i in
        (o, Some (i, o)))
  in loop co None

let memo (type a) (f : a -> 'b) =
  let module Map = Map.Make(struct
    type t = a
    let compare = compare
  end) in
  let co = arr (function (i, mem) ->
    try
      ( Map.find i mem, mem )
    with Not_found ->
      let o = f i in
      ( o, Map.add i o mem ) )
  in loop co Map.empty
