(* Chimpanzee Memory Test
 https://www.youtube.com/watch?v=zsXP8qeFF6A

 Copyright (C) 2020 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

let width, height = (64, 64)

let blue   = (0, 0, 255)
let green  = (0, 255, 0)
let yellow = (255, 255, 0)
let white  = (255, 255, 255)
let black  = (0, 0, 0)
let alpha  = 255

let numbers = [
  '0', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '1', [|
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '2', [|
    [| 1; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  '3', [|
    [| 1; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 1; 1; 1; 0; 0 |];
  |];
  '4', [|
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 1; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
  |];
  '5', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 0; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  '6', [|
    [| 0; 0; 1; 1; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '7', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  '8', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '9', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 1 |];
    [| 0; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
]


let src_rect = Rect.make4 0 0 5 5


let display renderer numbers_tex ns =
  Render.set_draw_color renderer black alpha;
  Render.clear renderer;

  let draw_number texture x y size =
    let dst_rect = Rect.make4 x y size size in
    Render.copy renderer ~texture ~src_rect ~dst_rect ();
  in
  List.iter (fun (n, (x, y)) ->
    let tex = List.assoc n numbers_tex in
    draw_number tex x y 5;
  ) ns;

  Render.render_present renderer;
;;


let proc_events = function
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | _ -> ()


let rec event_loop () =
  match Event.poll_event () with
  | None -> ()
  | Some ev ->
      let _ = proc_events ev in
      event_loop ()


let rec main_loop renderer numbers_tex ns =
  let t = Timer.get_ticks () in

  let _ = event_loop () in

  display renderer numbers_tex ns;

  let t2 = Timer.get_ticks () in
  let dt = t2 - t in

  Timer.delay (max 0 (40 - dt));

  main_loop renderer numbers_tex ns


let pixel_for_surface ~surface ~rgb =
  let fmt = Surface.get_pixelformat_t surface in
  let pixel_format = Pixel.alloc_format fmt in
  let pixel = Pixel.map_RGB pixel_format rgb in
  Pixel.free_format pixel_format;
  (pixel)


let texture_of_pattern renderer pattern ~color =
  let surface = Surface.create_rgb ~width:5 ~height:5 ~depth:32 in
  let rgb = (255, 255, 255) in
  let key = pixel_for_surface ~surface ~rgb in
  Surface.set_color_key surface ~enable:true ~key;
  let color = pixel_for_surface ~surface ~rgb:color in
  Array.iteri (fun y row ->
    Array.iteri (fun x v ->
      if v = 1
      then Surface.fill_rect surface (Rect.make4 x y 1 1) color
      else Surface.fill_rect surface (Rect.make4 x y 1 1) 0xFFFFFFl
    ) row
  ) pattern;
  let texture = Texture.create_from_surface renderer surface in
  Surface.free surface;
  (texture)


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer ~width:(width*2) ~height:(height*2) ~flags:[]
  in
  Render.set_logical_size2 renderer width height;
  Window.set_title ~window ~title:"Chimp Test";

  let numbers_tex =
    List.map (fun (c, pat) ->
      let tex = texture_of_pattern renderer pat ~color:green in
      (c, tex)
    ) numbers
  in
  let ns =
    List.init 8 (fun i ->
      let n = succ i in
      let x = Random.int (width - 5) in
      let y = Random.int (height - 5) in
      let c = (Printf.sprintf "%d" n).[0] in
      (c, (x, y))
    )
  in

  main_loop renderer numbers_tex ns
