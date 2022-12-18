open Vector

let gravity : vector = vec_from_polar 9.8 (Float.pi /. -2.)

(** 30fps *)
let dt : float = 2.06 /. 50.

let h : float = 1. /. 3000000.
let runtime : float = 3.
