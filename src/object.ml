type pos = {
  x : float;
  y : float;
}
(**Position is center if Circle and Top left if rectangle. *)

type circle_dim = { radius : float }

type rect_dim = {
  height : float;
  width : float;
}

type obj_dim =
  | Circle of circle_dim
  | Rect of rect_dim

type prop = {
  mass : float;
  velocity : Physmath.Vector.vector;
  is_static : bool;
}

type obj = {
  position : pos;
  shape : obj_dim;
  properties : prop;
  forces : Physmath.Vector.vector list;
}
(** An object has a position, dimensions, and properties *)

(** [create_circle r] creates a circle of radius r *)
let create_circle r = Circle { radius = r }

(** [create_rect h w] creates a rectangle with height h and width w*)
let create_rect h w = Rect { height = h; width = w }

(** [make x' y' shape m' ?forces] makes a new object with x pos x'; y pos y';
    shape of type obj_dim; mass m'; init velocity v'; and applied forces ?forces *)
let make (x' : float) (y' : float) (shape : obj_dim) (m' : float) ?(forces = [])
    (v' : Physmath.Vector.vector) : obj =
  {
    position = { x = x'; y = y' };
    shape;
    properties = { mass = m'; velocity = v'; is_static = false };
    forces;
  }

(** [xpos obj] is a helper function designed to find the x position of a object
    more quickly.*)
let xpos (obj : obj) : float = obj.position.x

(** [ypos obj] is a helper function designed to find the position of a object
    more quickly.*)
let ypos (obj : obj) : float = obj.position.y

(** [rad obj] is a helper function designed to find the radius of a object more
    quickly.*)
let rad (obj : obj) : float =
  match obj.shape with
  | Circle { radius } -> radius
  | _ -> failwith "This object has no radius as it is not a circle"

let height (obj : obj) : float =
  match obj.shape with
  | Rect { height; width } -> height
  | _ -> failwith "This object has no height as it is not a Rectangle"

let width (obj : obj) : float =
  match obj.shape with
  | Rect { height; width } -> width
  | _ -> failwith "This object has no width as it is not a Rectangle"

let isRect (obj : obj) : bool =
  match obj.shape with
  | Rect { height; width } -> true
  | _ -> false

let isCircle (obj : obj) : bool =
  match obj.shape with
  | Circle { radius } -> true
  | _ -> false

let equals (objA : obj) (objB : obj) : bool =
  objA.position = objB.position
  && objA.shape = objB.shape
  && objA.properties = objB.properties
  && objA.forces = objB.forces

let shape obj = obj.shape

(*[pointmass] Makes a point particle*)
let pointmass : obj_dim = Circle { radius = 0. }
let ball : obj_dim = Circle { radius = 10. }
let unit_ball : obj_dim = Circle { radius = 1. }
let plinko_peg : obj_dim = Circle { radius = 0.1 }
let square : obj_dim = Rect { height = 10.; width = 10. }

(*[floor] makes an object that is the floor of a certain environment. *)
let floor : obj_dim = Rect { height = 5.; width = 100. }

(** [mass obj] is a helper function designed to find the mass of a object more
    quickly.*)
let mass (obj : obj) : float = obj.properties.mass

(** [vel obj] is a helper function designed to find the velocity of a object
    more quickly.*)
let vel obj = obj.properties.velocity

(**[is_static obj] returns the boolean property whether or not an object is
   static*)
let is_static obj : bool = obj.properties.is_static

let make_static obj : obj =
  {
    position = obj.position;
    shape = obj.shape;
    properties =
      { mass = 0.; velocity = Physmath.Vector.zero_vector; is_static = true };
    forces = [];
  }

let forces ob = ob.forces

(** [update_x_vel vi_x a_x] returns the x component of velocity after
    Physmath.Constants.dt i.e. [ v(t) = v_0 + at ]*)
let update_x_vel vi_x a_x = vi_x +. (a_x *. Physmath.Constants.dt)

(** [update_y_vel vi_x a_x] returns the y component of velocity after
    Physmath.Constants.dt i.e. [ v(t) = v_0 + at ]*)
let update_y_vel vi_y a_y = vi_y +. (a_y *. Physmath.Constants.dt)

(** [update_x_pos x_i v_x a_x] returns the x position after
    Physmath.Constants.dt with initial values x_i v_i and accel a [ x(t) ]*)
let update_x_pos x_i v_x a_x =
  x_i
  +. (v_x *. Physmath.Constants.dt)
  +. (0.5 *. a_x *. Physmath.Constants.dt *. Physmath.Constants.dt)

(** [update_y_pos y_i v_y a_y] returns the y position after
    Physmath.Constants.dt with initial values x_i v_i and accel a [ x(t) ]*)
let update_y_pos y_i v_y a_y =
  y_i
  +. (v_y *. Physmath.Constants.dt)
  +. (0.5 *. a_y *. Physmath.Constants.dt *. Physmath.Constants.dt)

(**[add_force obj force] takes in an object and a force and returns a new force
   that is exactly the same except with the force put into the forces list in
   the object.*)
let add_force (obj : obj) (force : Physmath.Vector.vector) : obj =
  make (xpos obj) (ypos obj) obj.shape (mass obj) (vel obj)
    ~forces:(obj.forces @ [ force ])

let obj_to_string ?(verbose = false) (obj : obj) : string =
  "Object has position ("
  ^ (xpos obj |> string_of_float)
  ^ ", "
  ^ (ypos obj |> string_of_float)
  ^ "). It's velocity is ("
  ^ (vel obj |> Physmath.Vector.mag |> string_of_float)
  ^ ", "
  ^ (vel obj |> Physmath.Vector.theta |> string_of_float)
  ^ "). It's type is "
  ^ (if isRect obj then "Rectangle." else "Circle.")
  ^
  if verbose then
    let net = forces obj |> Physmath.Vector.net_vec in
    " mass = "
    ^ (mass obj |> string_of_float)
    ^ "; radius = "
    ^ (rad obj |> string_of_float)
    ^ "; [Net Force] = ("
    ^ (net |> Physmath.Vector.mag |> string_of_float)
    ^ ", "
    ^ (net |> Physmath.Vector.theta |> string_of_float)
    ^ ")"
  else ""
