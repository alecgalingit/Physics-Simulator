(** OSCILATIONS is a module that simulates a mass-spring-damper system *)

type y
(** y is the rep type of an equation of the form y(t) *)

type forcing_function
(** forcing_function is the adt of a forcing function i.e. the RHS of second
    order ODE in Canonical form Ay'' + By' + y = F(t) *)

val y_t : float -> y -> float
(** [y_t t y] is y(t) *)

val make : float -> float -> float -> forcing_function -> float -> float -> y
(** [make m c k F(t) y y'] is the y(t) without the constants determined yet *)

val zero : forcing_function
(** [zero] is if F(t) = 0 *)

val grav : forcing_function
(** [grav] is if F(t) = gravity *)

val sin : float -> float -> forcing_function
(** [sin amp freq] is F(t) = ampSIN (freq t) *)

val cos : float -> float -> forcing_function
(** [cos amp freq] is F(t) = ampCOS (freq t) *)
