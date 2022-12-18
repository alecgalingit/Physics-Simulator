type state = {
  objs : Object.obj list;
  environment : Environment.env;
  time : float;
  collide : bool;
  springs : (string * float * Oscillations.y) list;
}

(** [time st] is the time of state st *)
let time st = st.time

let objs st = st.objs

let width st = Environment.width st.environment 

let height st = Environment.height st.environment 

(** [update_obj obj] updates one object *)
let update_obj (obj : Object.obj) : Object.obj =
  let open Object in
  let open Physmath.Vector in
  let net_force = forces obj |> net_vec in
  let accel = vec_from_polar (mag net_force /. mass obj) (theta net_force) in
  make (* x pos *)
    (update_x_pos (xpos obj) (vel obj |> x_of_vector) (x_of_vector accel))
    (* y pos *)
    (update_y_pos (ypos obj) (vel obj |> y_of_vector) (y_of_vector accel))
    (shape obj)
    (* radius *) (mass obj) (* mass *)
    (* velocity *)
    (vec_from_cartesian
       (update_x_vel (vel obj |> x_of_vector) (x_of_vector accel))
       (update_y_vel (vel obj |> y_of_vector) (y_of_vector accel)))
    (* forces *)
    ~forces:(forces obj)

(**[update_obj obj] takes in an object and updates all its attributes based on
   an acceleration that it is experiencing at a specific moment in time.
   Determines acceleration using net_force, a helper which sums all the forces
   that an object is experiencing together*)

let update_objs (objs : Object.obj list) : Object.obj list =
  List.map update_obj objs

let two_rect_overlap objA objB : bool =
  let open Object in
  let yOverlap =
    ypos objA <= ypos objB +. height objB
    && ypos objA +. height objA >= ypos objB
  in
  let xOverlap =
    xpos objA <= xpos objB +. width objB && xpos objA +. width objA >= xpos objB
  in
  yOverlap && xOverlap

let posHelper circle vEdge hEdge : bool =
  let open Object in
  let distX = xpos circle -. vEdge in
  let distY = ypos circle -. hEdge in
  let dist = sqrt ((distX *. distX) +. (distY *. distY)) in
  dist <= rad circle

(**[one_circle_overlap objA objB] checks to see if one rectangle and one circle
   are overlapping. A is circle, B is rectangle.*)
let one_circle_overlap objA objB : bool =
  let open Object in
  let open Physmath.Vector in
  let centerRectPos =
    vec_from_cartesian
      (xpos objA +. (width objA /. 2.))
      (ypos objA +. (height objA /. 2.))
  in
  let centerCirclePos = vec_from_cartesian (xpos objB) (ypos objB) in
  let differencePos = centerCirclePos >- centerRectPos in
  let exteriorPos =
    centerRectPos
    >- clamp_vec differencePos
         (Float.neg (width objA /. 2.), width objA /. 2.)
         (Float.neg (height objA /. 2.), height objA /. 2.)
  in
  if
    xpos objB +. rad objB > x_of_vector exteriorPos
    && ypos objB +. rad objB > y_of_vector exteriorPos
  then true
  else false

  let two_circle_overlap objA objB =
    let open Object in
    let open Physmath.Vector in
    let centerCircleA = vec_from_cartesian (xpos objA) (ypos objA) in
    let centerCircleB = vec_from_cartesian (xpos objB) (ypos objB) in
    (* B to A *)
    let differencePos = centerCircleA >- centerCircleB in
    let sumRadius = rad objA +. rad objB in
    mag differencePos *. mag differencePos <= sumRadius *. sumRadius
(**[collide objA objB] checks two individual object's for collisions using
   helpers. *)
let collide (objA : Object.obj) (objB : Object.obj) : bool =
  let open Object in
  if Object.equals objA objB then false
  else if isRect objA then
    if isRect objB then two_rect_overlap objA objB
    else one_circle_overlap objB objA
  else if isRect objB then one_circle_overlap objA objB
  else two_circle_overlap objA objB
(* TODO: which one is it? *)

(** [check_collision obj objs] checks any singular object versus a list of other
    objects to see if they collide. *)
let rec check_collision (obj : Object.obj) (objs : Object.obj list) : bool =
  match objs with
  | [] -> false
  | h :: t -> if collide obj h then true else check_collision obj t

(** [check_collision check_list objs] takes the list of object who's collisions
    need to be checked with the full object list in the current state. *)
let rec check_collisions check_list (objs : Object.obj list) : bool =
  match check_list with
  | [] -> false
  | h :: t -> if check_collision h objs then true else check_collisions t objs

let two_rect_resolver objA objB = failwith "unimplemented"
(* let open Object in let open Vector in let xAxis = vec_from_cartesian 1. 0. in
   let yAxis = vec_from_cartesian 0. 1. in let xProjA = ( dot
   (vec_from_cartesian (xpos objA) (ypos objA)) xAxis, dot (vec_from_cartesian
   (xpos objA +. width objA) (ypos objA +. height objA)) xAxis ) in let xProjB =
   ( dot (vec_from_cartesian (xpos objB) (ypos objB)) xAxis, dot
   (vec_from_cartesian (xpos objB +. width objB) (ypos objB +. height objB))
   xAxis ) in let xOverlap = overlap_helper xProjA xProjB in let yOverlap =
   overlap_helper yProjA yProjB in if xOverlap < yOverlap then [ make (xpos objA
   +. xOverlap) (ypos objA) (shape objA) (mass objA) ~forces:(forces objA) (vel
   objA); make (xpos objB) (ypos objB) (shape objB) (mass objB) ~forces:(forces
   objB) (vel objB); ] else [ make (xpos objA) (ypos objA +. yOverlap) (shape
   objA) (mass objA) ~forces:(forces objA) (vel objA); make (xpos objB) (ypos
   objB) (shape objB) (mass objB) ~forces:(forces objB) (vel objB); ] *)

(**[static_resolver static obj] is the resolver which updates obj's properties
   based on its collision with static. *)
let static_resolver static obj : Object.obj list = failwith "Unimplemented"

let two_circle_resolver objA objB = failwith "Unimplemented"

(**[one_circle_resolver objA objB] is a helper that resolves the collision
   between a circle and a rectangle by returning an updated object list. objA is
   the circle and objB is the rectanlge*)
let one_circle_resolver objA objB = failwith "Unimplemented"

(**[resolve objA objB] takes in two objects that are in collision with
   eachother, and resolves the collision between them*)
let resolve (objA : Object.obj) (objB : Object.obj) : Object.obj list =
  let open Object in
  if Object.equals objA objB then [ objA ]
  else if is_static objA then
    if is_static objB then [ objA; objB ]
    else static_resolver objA objB (* First object is the static one. *)
  else if is_static objB then static_resolver objB objA
  else if isRect objA then
    if isRect objB then two_rect_resolver objA objB
    else one_circle_resolver objB objA
  else if isRect objB then one_circle_resolver objA objB
  else two_circle_resolver objA objB

(**[cr_find_collision obj objs] is a helper which takes in an object and a list
   of objects, and returns an object list with all the collisions resolved, if
   there were any.*)
let rec cr_find_collision (obj : Object.obj) (objs : Object.obj list) :
    Object.obj list =
  match objs with
  | [] -> []
  | h :: t ->
      if collide obj h then resolve obj h @ cr_find_collision obj t
      else cr_find_collision obj t

(**[cr_find_collisions check_list collided_list objs] is a helper which takes in
   3 object list arguments and returns an updated object list with all
   collisions resolved.*)
let rec cr_find_collisions check_list collided_list (objs : Object.obj list) :
    Object.obj list =
  match check_list with
  | [] -> objs
  | h :: t ->
      cr_find_collisions t (collided_list @ cr_find_collision h objs) objs

(**[make objs springs environment ?time] creates a state given the parameters it
   is given. Additionally, it checks for collisions when the state is made. *)
let make ?(time = 0.) objs springs environment =
  { objs; environment; time; collide = check_collisions objs objs; springs }

(** [collision_resolution r] is a helper which takes in a state and outputs the
    state, with any collisions that might have occured resolved. *)
let collision_resolution (state : state) =
  make (cr_find_collisions state.objs [] state.objs) state.springs state.environment

let rec print_loop_objs (obs : Object.obj list) : string =
  match obs with
  | [] -> "\n"
  | h :: t ->
      "  " ^ Object.obj_to_string h ^ "\n" ^ print_loop_objs t

let rec print_loop_springs (sps : (string * float * Oscillations.y) list) :
    string =
  match sps with
  | [] -> "\n"
  | (name, displacement, spring) :: t ->
      "  " ^ name ^ " has displacement from rest of: "
      ^ string_of_float displacement
      ^ "\n" ^ print_loop_springs t

let state_to_string (st : state) : string =
  "State Time: "
  ^ (st.time |> string_of_float)
  ^ "\n" ^ "State Environment: \n" ^ "  collisions: "
  ^ string_of_bool st.collide ^ "\n" ^ "  width: "
  ^ (st.environment |> Environment.width |> string_of_int)
  ^ "px\n" ^ "  height: "
  ^ (st.environment |> Environment.height |> string_of_int)
  ^ "px\n\n" ^ "State Objects: \n"
  ^ print_loop_objs st.objs
  ^ begin
      if List.length st.springs = 0 then "No Springs in State"
      else "State Springs: \n" ^ print_loop_springs st.springs
    end
  ^ "\n*******************\n\n"

(* then "\n" ^ "State in Collision!!" else "" *)

let update_springs springs t =
  let rec aux s t' acc =
    match s with
    | [] -> acc
    | (name, position, spring) :: rest ->
        aux rest t' ((name, Oscillations.y_t t' spring, spring) :: acc)
  in
  aux springs t []

(**[step_time state dt] steps the time forward by an amount dt every time it is
   run. This updates all object internal to the environment.*)
let step_time (state : state) : state =
  let open Physmath.Constants in
  if state.collide then
    let collisions_resolved = (*collision_resolution*) state in
    make
      ~time:(collisions_resolved.time +. dt)
      (update_objs collisions_resolved.objs)
      state.springs collisions_resolved.environment
  else
    make ~time:(state.time +. dt) (update_objs state.objs)
      (update_springs state.springs (state.time +. dt))
      state.environment
(* if collision_detection state then (* make ~time:(state.time +. dt)
   (update_objs state.objs) state.environment *) (* replace state with :
   collision_state_update *) (* Collision_state_update method will have to
   process the collision that has happened in order to update all attributes of
   the two. Maybe should call collision update inside one of the collision
   helpers so that we know which things have to be adjusted instead of the whole
   list.*) (* else make ~time:(state.time +. dt) (update_objs state.objs)
   state.environment *) *)

let return_collide (state : state) = state.collide
