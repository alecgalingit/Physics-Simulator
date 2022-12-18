type vector = {
  mag : float;
  theta : float;
}

let mag v = v.mag
let theta v = v.theta
let x_of_vector v = v.mag *. Float.cos v.theta
let y_of_vector v = v.mag *. Float.sin v.theta

let vec_from_cartesian x y =
  { mag = Float.sqrt ((x *. x) +. (y *. y)); theta = Float.atan (y /. x) }

let vec_from_polar mag theta =
  if mag >= 0. then { mag; theta }
  else { mag = -1. *. mag; theta = theta +. Float.pi }

let zero_vector = vec_from_polar 0. Float.pi
let x_unit_vector = vec_from_cartesian 1. 0.
let y_unit_vector = vec_from_cartesian 0. 1.

let ( +> ) x y =
  if x.mag <> 0. && y.mag <> 0. then
    let sum_x = x_of_vector x +. x_of_vector y in
    let sum_y = y_of_vector x +. y_of_vector y in
    vec_from_cartesian sum_x sum_y
  else if x.mag = 0. then y
  else x

let ( >- ) x y =
  if x.mag <> 0. && y.mag <> 0. then
    let sum_x = x_of_vector x -. x_of_vector y in
    let sum_y = y_of_vector x -. y_of_vector y in
    vec_from_cartesian sum_x sum_y
  else if x.mag = 0. then y
  else x

(** Scalar multiplication. *)
let ( *> ) x' s = vec_from_polar (x'.mag *. s) x'.theta

(** [clamp value min max] restricts a value to be within a certain range if it
    is outside.*)
let clamp (value : float) (min : float) (max : float) : float =
  Float.max min (Float.min max value)

(** [clamp_vec vec x_range y_range] returns a new vector which has been
    restricted to fit within a certain 2d area*)
let clamp_vec vec x_range y_range =
  vec_from_cartesian
    (clamp (x_of_vector vec) (fst x_range) (snd x_range))
    (clamp (y_of_vector vec) (fst y_range) (snd y_range))

(** [net_vec_aux vc acc] is the recursive helper function which folds through
    the list of vectors being applied to an object in order to determine the net
    force using vector addition.*)
let rec net_vec_aux vc acc =
  match vc with
  | [] -> acc
  | head :: tail -> head +> acc |> net_vec_aux tail

let net_vec vecs = net_vec_aux vecs { mag = 0.; theta = 0. }

let dot x y =
  (x_of_vector x *. x_of_vector y) +. (y_of_vector x *. y_of_vector y)

(** Scalar multiplication. *)
let ( *> ) x y = vec_from_polar (mag x *. y) (theta x)
