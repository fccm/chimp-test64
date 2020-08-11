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

type pos = int * int  (* position (x, y) *)

type test_state =
  | Visualise of (char * int * pos) list
  | Hidden of int * (char * int * pos) list

exception Game_Over

let width, height = (320, 240)
let num_size = 15

let blue   = (0, 0, 255)
let green  = (0, 255, 0)
let red    = (255, 0, 0)
let yellow = (255, 255, 0)
let white  = (255, 255, 255)
let black  = (0, 0, 0)
let alpha  = 255

let numbers_pat = [
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


let display renderer numbers_tex test_state =
  Render.set_draw_color renderer black alpha;
  Render.clear renderer;

  let draw_number texture x y size =
    let dst_rect = Rect.make4 x y size size in
    Render.copy renderer ~texture ~src_rect ~dst_rect ();
  in

  let draw_square x y size =
    let rect = Rect.make4 x y size size in
    Render.set_draw_color renderer green alpha;
    Render.fill_rect renderer rect;
  in

  begin match test_state with
  | Visualise ns ->
      List.iter (fun (c, n, (x, y)) ->
        let tex = List.assoc c numbers_tex in
        draw_number tex x y num_size;
      ) ns;
  | Hidden (n, ns) ->
      List.iter (fun (c, n, (x, y)) ->
        draw_square x y num_size;
      ) ns;
  end;

  Render.render_present renderer;
;;


let proc_events = function
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0

  | Event.Mouse_Button_Down mouse_button_event ->
      let x = mouse_button_event.Event.mb_x in
      let y = mouse_button_event.Event.mb_y in
      Some (x, y)

  | _ -> None


let rec event_loop () =
  match Event.poll_event () with
  | None -> None
  | Some ev ->
      match proc_events ev with
      | None -> event_loop ()
      | click -> click


let point_in_rect ~p:(x, y) ~r =
  ( (x >= r.Rect.x) && (x < (r.Rect.x + r.Rect.w)) &&
    (y >= r.Rect.y) && (y < (r.Rect.y + r.Rect.h)) )


let does_hit n (x, y) ns =
  match ns with
  | (_, _n, (_x, _y)) :: [] ->
      let r = Rect.make4 _x _y num_size num_size in
      if (*Rect.*)point_in_rect ~p:(x, y) ~r
      then (print_endline "Congratulation!"; Sdl.quit (); exit 0)
      else raise Game_Over

  | (_, _n, (_x, _y)) :: tail ->
      let r = Rect.make4 _x _y num_size num_size in
      _n = n && (*Rect.*)point_in_rect ~p:(x, y) ~r
  | [] ->
      assert false


let update_state event test_state =
  match event with
  | None -> test_state
  | Some (x, y) ->
      match test_state with
      | Visualise ns ->
          if does_hit 1 (x, y) ns
          then Hidden (2, List.tl ns)
          else raise Game_Over
      | Hidden (n, ns) ->
          if does_hit n (x, y) ns
          then Hidden (succ n, List.tl ns)
          else raise Game_Over


let rec main_loop renderer numbers_tex test_state =
  let t = Timer.get_ticks () in

  let event = event_loop () in

  let test_state = update_state event test_state in

  display renderer numbers_tex test_state;

  let t2 = Timer.get_ticks () in
  let dt = t2 - t in

  Timer.delay (max 0 (50 - dt));

  main_loop renderer numbers_tex test_state


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


let collide (_, _, (x1, y1)) (_, _, (x2, y2)) =
  let a = {
    Rect.x = x1;
    Rect.y = y1;
    Rect.w = num_size;
    Rect.h = num_size;
  } and b = {
    Rect.x = x2;
    Rect.y = y2;
    Rect.w = num_size;
    Rect.h = num_size;
  } in
  Rect.has_intersection ~a ~b


let collision num numbers =
  List.exists (fun number ->
    collide num number
  ) numbers


let new_number i =
  let n = succ i in
  let x = Random.int (width - num_size) in
  let y = Random.int (height - num_size) in
  let c = (Printf.sprintf "%d" n).[0] in
  (c, n, (x, y))


let init_numbers n =
  let rec aux i acc =
    if i >= n then (List.rev acc) else
      let number = new_number i in
      (* check that no number initially collide with another one *)
      if collision number acc then aux i acc
      else aux (succ i) (number :: acc)
  in
  aux 0 []


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  Render.set_logical_size2 renderer width height;
  Window.set_title ~window ~title:"Chimp Test";

  let numbers_tex =
    List.map (fun (c, pat) ->
      let tex = texture_of_pattern renderer pat ~color:green in
      (c, tex)
    ) numbers_pat
  in
  let ns = init_numbers 9 in
  let test_state = Visualise ns in

  main_loop renderer numbers_tex test_state
