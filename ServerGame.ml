open Coroutine

let run_steps step dt =
  let rec steps acc step time =
    if time >= dt then
      let res, step' = runC step time in
      steps (res :: acc) step' (time -. dt)
    else
      (List.rev acc, step, time)
  in
  let rec mk_co step rem =
  { coroutine = fun time ->
    let res, step', rem' = steps [] step (time +. rem) in
    ( res, mk_co step' rem' ) }
  in mk_co step 0.0

let messages = run_steps read_stack 1.0

let game =
  messages >>> { coroutine = fun msgs ->

(* 
 * render (); think (input ()) * {n};
 *)
