(** [draw_rect obj] draws a rectangular object obj, returning unit. *)
let draw_rect obj =
  GlDraw.rect
    (Phys.Object.xpos obj, Phys.Object.ypos obj)
    ( Phys.Object.xpos obj +. Phys.Object.width obj,
      Phys.Object.ypos obj +. Phys.Object.height obj )

(** [draw_circle obj] draws a circular object obj, returning unit. *)
let draw_circle obj =
  let twicePi = 2.0 *. Float.pi in
  let triangleAmount = 200 in
  GlDraw.begins `triangle_fan;
  GlDraw.vertex ~x:(Phys.Object.xpos obj) ~y:(Phys.Object.ypos obj);
  for i = 0 to triangleAmount do
    GlDraw.vertex
      ~x:
        (Phys.Object.xpos obj
        +. Phys.Object.rad obj
           *. cos (float_of_int i *. twicePi /. float_of_int triangleAmount))
      ~y:
        (Phys.Object.ypos obj
        +. Phys.Object.rad obj
           *. sin (float_of_int i *. twicePi /. float_of_int triangleAmount))
      ()
  done;
  GlDraw.ends ()

(** [draw_object obj] draws an object obj, returning unit. *)
let draw_object obj =
  if Phys.Object.isRect obj then draw_rect obj
  else if Phys.Object.isCircle obj then draw_circle obj

(** [reshape w h] reshapes the Glut window as it's width and height changes,
    returning unit. *)
let reshape w h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  GlClear.color (1.0, 1.0, 1.0);
  GlClear.clear [ `color ];
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(-1.0, 1.0) ~y:(-1.0, 1.0) ~z:(-1.0, 1.0)

(** [draw_objects objects] maps draw_object to a list of objects. *)
let rec draw_objects objects =
  match objects with
  | [] -> ()
  | obj :: t ->
      draw_object obj;
      draw_objects t

(** [main st]] creates a graphic representation of state st. *)
let main st =
  let st_to_pass_in = ref st in

  let time_passes () = st_to_pass_in := Phys.State.step_time !st_to_pass_in in

  let draw_state st =
    GlDraw.color (0.0, 0.0, 0.0);
    GlClear.clear [ `color ];
    draw_objects (Phys.State.objs !st_to_pass_in);
    Glut.swapBuffers ()
  in

  let idle_state st =
    time_passes ();
    draw_state st
  in
  let _reshape ~w ~h = reshape w h in
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w:(Phys.State.width st) ~h:(Phys.State.height st);
  ignore (Glut.createWindow ~title:"Physics Engine");
  Glut.reshapeFunc ~cb:_reshape;
  Glut.displayFunc ~cb:(fun () -> draw_state st);
  Glut.idleFunc (Some (fun () -> idle_state st));
  Glut.mainLoop ()
