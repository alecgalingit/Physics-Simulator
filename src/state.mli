(** A module representing the current state of the simulation, that is a single
    frame too be drawn by the graphics engine where all objects have been
    updated. *)

type state
(** The State of the simulation *)

val objs : state -> Object.obj list
(** [objs st] returns a list of the objects in st *)

val width : state -> int
(** [width st] returns the width of the environment of st *)

val height : state -> int
(** [height st] returns the height of the environment of st *)

val make :
  ?time:float ->
  Object.obj list ->
  (string * float * Oscillations.y) list ->
  Environment.env ->
  state
(** [make t_0 objs env] if objs is a Object.obj list and env is an
    Environment.env then this makes a new state with these attributes t_0 is
    optional defaults to 0*)

val time : state -> float
(** [time st] is the time of state st *)

val state_to_string : state -> string
(** [state_to_string st] is the string representation of state *)

val step_time : state -> state
(**[step_time state dt] steps the time forward by an amount dt every time it is
   run. This updates all object internal to the state.*)

val return_collide : state -> bool
(**[return_collide state] steps the time forward by an amount dt every time it
   is run. This updates all object internal to the state.*)

val check_collisions : Object.obj list -> Object.obj list -> bool

val two_circle_overlap : Object.obj -> Object.obj -> bool
(**[two_circle_overlap2 objA objB] checks to see if two rectagles are
   overlapping. A is circle, B is circle.*)

val one_circle_overlap : Object.obj -> Object.obj -> bool
(**[one_circle_overlap objA objB] checks to see if one rectangle and one circle
   are overlapping. A is circle, B is rectangle.*)

val two_rect_overlap : Object.obj -> Object.obj -> bool
(**[two_rect_overlap objA objB] checks to see if two rectagles are overlapping.
   A is rect, B is rect.*)
