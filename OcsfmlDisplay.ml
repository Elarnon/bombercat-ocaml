open Lwt
open Misc
open Data
open OcsfmlWindow
open OcsfmlGraphics
open Protocol.Game

let cat = new texture
  ~rect:({ left = 0; top = 0; width = 96; height = 128 })
  (`File "animals.png")

let tcrate = new texture
  ~rect:({left = 0; top = 0; width = 50; height = 50 })
  (`File "crates.png")

let tbg = new texture (`File "background.png")

let bombs = new texture (`File "bombs.png")

let boum n = new texture (`File ("explosion" ^ string_of_int n ^ ".png"))

let boums = List.map boum [0; 1; 2; 3]

let mk_sprite y x (px,py) =
  new sprite
    ~texture:cat
    ~texture_rect:({ left = y * 32; top = x * 32; width = 32; height = 32 })
    ~scale:(0.5,0.5)
    ~position:(float_of_int px *. 16., float_of_int py *. 16.)
    ()

let mk_bomb n (x,y) =
  new sprite
    ~texture:bombs
    ~texture_rect:({ left = n * 16; top = 0; width = 16; height = 16 })
    ~position:(float_of_int x *. 16., float_of_int y *. 16.)
    ()

(* let mk_boum n (x,y) =
  new sprite
    ~texture:(List.nth boums n)
    ~texture_rect:( )
    ~position:(float_of_int x *. 16., float_of_int y *. 16.)
    () *)

let mk_up = mk_sprite 0
let mk_left = mk_sprite 1
let mk_right = mk_sprite 2
let mk_down = mk_sprite 3

let mk_crate (x,y) =
  new sprite
    ~texture:tcrate
    ~texture_rect:({ left = 0; top = 0; width = 50; height = 50 })
    ~scale:(16./.50., 16./.50.)
    ~position:(float_of_int x *. 16., float_of_int y *. 16.)
    ()

let mk_empty (x,y) =
  new sprite
    ~texture:tbg
    ~texture_rect:({ left = 0; top = 0; width = 16; height = 16 })
    ~position:(float_of_int x *. 16., float_of_int y *. 16.)
    ()

let mk_wall (x,y) =
  new sprite
    ~texture:tbg
    ~texture_rect:({ left = 6 * 16; top = 0; width = 16; height = 16 })
    ~position:(float_of_int x *. 16., float_of_int y *. 16.)
    ()

let mk position = function
  | Empty -> mk_empty position
  | Destructible -> mk_crate position
  | Indestructible -> mk_wall position
  | Bomb t -> mk_bomb 0 position

type t =
  { map : Data.map
  ; params : Protocol.Initialisation.params
  ; me : char
  ; app : render_window
  }

let create map params me =
  let width = Data.width map * 16
  and height = Data.height map * 16 in
  let vm = VideoMode.({ width; height; bits_per_pixel = 32 }) in
  let app = new render_window vm "Bombercat client v0.1" in
  return { map; params; me; app }

let update { map; params; me; app } turn =
  app#clear ();
  Data.iter_content (fun pos c ->
    app # draw (mk_empty pos);
    app # draw (mk pos c)) map;
  Data.iter_players (fun c pos ->
    let sprite = mk_up 0 pos in
    app # draw sprite) map;
  app # display

let rec input { map; params; me; app } =
  Lwt_main.yield () >>= fun () ->
  let open Event in
  match app#poll_event with
  | Some e -> begin match e with
    | Closed -> return_none
    | KeyPressed { code = KeyCode.Escape } ->
        return_none
    | KeyPressed { code } -> begin try
      let pos = Data.map_pos map me in
      let open KeyCode in
      match code with
      | Left ->
          return @$ Some (MOVE (pos, Data.Left))
      | Up ->
          return @$ Some (MOVE (pos, Data.Up))
      | Down ->
          return @$ Some (MOVE (pos, Data.Down))
      | Right ->
          return @$ Some (MOVE (pos, Data.Right))
      | B ->
          return @$ Some (BOMB pos)
      | _ -> input { map; params; me; app }
    with Not_found -> input { map; params; me; app } end
    | _ -> input { map; params; me; app }
  end
  | None ->
        input { map; params; me; app }

let quit { map; params; me; app } =
  app # close; return ()
