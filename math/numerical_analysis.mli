(** A module to preform fast numerical analysis on first order and second order
    ordinary differntial equations. Reaches low levels of error via RK4 method
    of fast numerical analysis.

    Types for results of first order differential equations: yt & f_yt. and
    second order differential equations: yy't and f_yy't

    Provides representations for a series of individual points as well as a
    single point. *)

(* ************************************************************************** *)
(* extra functions included, need to implement *)

type yt = float * float
(** ADT for a single point in the solution to the First Order Ordinary
    Differential Equation's Initial Value Problem

    Accsess values with [time yt] and [y yt] *)

type yy't = float * float * float
(** ADT for a single point in the solution to the Second Order Ordinary
    Differential Equation's Initial Value Problem

    Accsess values with [time_second yy't] and [y_second yy't] [y'_second yy't] *)

val first_order : (float -> float -> float) -> float -> float -> yt
(** [first_order y'(t) t(0) y(0)] if y'(t) = f(t,y) and t(0), y(0) are the
    initial values of the equation, this returns the next point of the solution
    after point (t(0), y(0)) Requires: y'(t) must be of the form y'(t) = f(t, y) *)

val second_order :
  (float -> float -> float -> float) -> float -> float -> float -> yy't
(** [second_order y''(t) t(0) y(0) y'(0))] if y''(t) = f(t,y,y') and t(0), y(0),
    y'(0) are the initial values of the equation, this returns the next point of
    the solution after point (t(0), y(0), y'(0)) Requires: y''(t) must be of the
    form y''(t) = f(t, y, y') *)

val time_first : yt -> float
(** [time_first point] gives the time associated with point *)

val y_first : yt -> float
(** [y_first point] gives the y value associated with point *)

val time_second : yy't -> float
(** [time_second point] gives the time associated with point *)

val y_second : yy't -> float
(** [y_second point] gives the y value associated with point *)

val y'_second : yy't -> float
(* * [y'_second point] gives the y' value associated with point *)

val f_ty : float -> float -> float -> float
(** [f_ty f t y] used to create a F(t, y) *)

val f_tyy' : float -> float -> float -> float -> float
(**[f_tyy' f t y y']used to create a F(t, y, y') *)