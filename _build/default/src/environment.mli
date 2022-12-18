(** Module for an Physics environment objects in the simulation are bounded by
    the environment. The Graphics Engine draws objects in the environment*)

type env
(** ADT representing the environment of the simulation *)

val width : env -> int
(** [width env] returns the width of the environment env *)

val height : env -> int
(** [height env] returns the height of the environment env *)

val make : int -> int -> env
(** [make width height] make an environment with width and height *)
