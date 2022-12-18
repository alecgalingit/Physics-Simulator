(** A Module of Constants to be used throughout the simulation for consistancy
    between calculations

    - gravity : acceleration due to gravity Vector.vector < 9.8, -pi/2 >
    - dt : time interval used
    - h : stepsize used in numerical analysis

    None of these values can be altered with either mutable or immutable methods *)

val gravity : Vector.vector
(** Acceleration due to gravity on standard xy basis *)

val dt : float
(** Time step used in simulation. Change in time or display time. Whatever this
    is set to is time between states/frames in the simulation*)

val h : float
(** Stepsize. Whatever this is set to is the number of itterations of numerical
    analysis per second*)

val runtime : float(** [runtime] is the total runtime of the simulation*)
