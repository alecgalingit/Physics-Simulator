(** Module representing a Physics Object *)

type obj
(** ADT representing an object *)

type obj_dim
(** ADT representing the dimensions of an object, i.e. rectangle or circle. Used
    in making an object.*)

val create_circle : float -> obj_dim
(** [create_circle r] creates a circle of radius r *)

val create_rect : float -> float -> obj_dim
(** [create_rect h w] creates a rectangle with height h and width w *)

val make :
  float ->
  float ->
  obj_dim ->
  float ->
  ?forces:Physmath.Vector.vector list ->
  Physmath.Vector.vector ->
  obj
(** [make x y shape m force_lst velocity] makes a new object with x pos x'; y
    pos y'; radius r'; mass m'; init velocity v'; and applied forces ?forces *)

val xpos : obj -> float
(** [xpos ob] if ob is an obj, this returns it's x position. For a circle this
    is it's center, for a rectangle this is it's bottom left edge.*)

val ypos : obj -> float
(** [ypos ob] if ob is an obj, this returns it's y position. For a circle this
    is it's center, for a rectangle this is it's bottom left edge.*)

val rad : obj -> float
(** [rad ob] if ob is an obj, this returns it's radius*)

val pointmass : obj_dim
(** [pointmass] is a point*)

val floor : obj_dim
(** [floor] is the floor object*)

val ball : obj_dim
(**[ball] is a ball with radius 10. *)

val unit_ball : obj_dim
(**[unit_ball] is a ball with radius 10. *)

val plinko_peg : obj_dim
(**[plinko_peg] is a circle with radius 0.1 *)

val square : obj_dim
(**[square] is a square with height and width 10. *)

val height : obj -> float
(** [height ob] if ob is an Rectangle, this returns it's height. else fail*)

val width : obj -> float
(** [height ob] if ob is an obj, this returns it's width. else fail*)

val shape : obj -> obj_dim
(** [shape ob] if ob is an obj, this returns it's shape.*)

val mass : obj -> float
(** [mass ob] if ob is an obj, this returns it's mass*)

val equals : obj -> obj -> bool

val isRect : obj -> bool
(** [isRect ob] if ob is an obj, this returns true if its shape is rectangular
    and false otherwise.*)

val isCircle : obj -> bool
(** [isCircle ob] if ob is an obj, this returns true if its shape is circular
    and false otherwise.*)

val vel : obj -> Physmath.Vector.vector
(** [vel ob] if ob is an obj, this returns it's velocity*)

val is_static : obj -> bool
(** [is_static obj] if ob is an obj, this returns whether or not it is static,
    i.e. how it collides with other objects. *)

val make_static : obj -> obj
(**[make_static obj] takes in an object, whether static or not, and makes it
   static.*)

val forces : obj -> Physmath.Vector.vector list
(** [forces ob] returns the list of vectors of ob *)

val update_x_pos : float -> float -> float -> float
(** [update_x_pos x_i v_x a_x] returns the x position after dt with initial
    values x_i v_i and accel a [ x(t) ]*)

val update_y_pos : float -> float -> float -> float
(** [update_y_pos y_i v_y a_y] returns the y position after dt with initial
    values x_i v_i and accel a [ x(t) ]*)

val update_x_vel : float -> float -> float
(** [update_x_vel vi_x a_x] returns the x component of velocity after dt i.e.
    [ v(t) = v_0 + at ]*)

val update_y_vel : float -> float -> float
(** [update_y_vel vi_y a_y] returns the y component of velocity after dt i.e.
    [ v(t) = v_0 + at ]*)

val add_force : obj -> Physmath.Vector.vector -> obj
(**[add_force obj force] takes in an object and a force and returns a new force
   that is exactly the same except with the force put into the forces list in
   the object.*)

val obj_to_string : ?verbose:bool -> obj -> string
(** [obj_to_string ob ?v] returns a string repr of ob. If v is true, it gives a
    verbsoe response *)
