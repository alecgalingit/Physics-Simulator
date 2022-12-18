(** A module to preform fast numerical analysis on first order and second order
    ordinary differntial equations. Reaches low levels of error via RK4 method
    of fast numerical analysis. Types for results of first order differential
    equations: yt & f_yt. and second order differential equations: yy't and
    f_yy't Provides representations for a series of individual points as well as
    a single point. *)

(* ************************************************************************** *)
type vector
(** ADT of a vector. In polar notation. A vector has a magnitude at an angle*)

val zero_vector : vector
(** The zero vector *)

val x_unit_vector : vector
(** x unit vector*)

val y_unit_vector : vector
(** y unit vector*)

val mag : vector -> float
(** [mag v] returns the magnitude of a vector*)

val theta : vector -> float
(** [theta v] returns the angle of the angle from the x axis in radians *)

val x_of_vector : vector -> float
(** [x_of_vector v] returns the x comp of vector v *)

val y_of_vector : vector -> float
(** [y_of_vector v] returns the y comp of vector v *)

val ( +> ) : vector -> vector -> vector
(** [x +> y] is the vector addition operation of vectors x and y *)

val ( >- ) : vector -> vector -> vector
(** [x >- y] is the vector subtraction operation of vectors x and y *)

val ( *> ) : vector -> float -> vector
(** [x' *> scalar] is the scalar multiplcation of vector x' and scalar *)

val vec_from_cartesian : float -> float -> vector
(** [vec_from_cartesian x y] returns the vector given two x y componets *)

val vec_from_polar : float -> float -> vector
(** [vec_from_polar r t] returns the vector given radius r and angle t componets *)

val clamp_vec : vector -> float * float -> float * float -> vector
(** [clamp_vec vec x_range y_range] returns a new vector which has been
    restricted to fit within a certain 2d area*)

val net_vec : vector list -> vector
(** [net_vec vec_list] if vec_list is a list of vectors, this will return the
    vector sum of all vectors in a list *)

val dot : vector -> vector -> float
(** [dot x y] returns the dot product of x and y *)

val ( *> ) : vector -> float -> vector
