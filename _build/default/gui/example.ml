let ex1 =
  Phys.State.make
    [
      Phys.Object.make 0. (-0.4)
        (Phys.Object.create_circle 0.2)
        1.
        (Physmath.Vector.vec_from_cartesian 0.005 0.);
      Phys.Object.make 0. 0.4
        (Phys.Object.create_circle 0.2)
        1.
        (Physmath.Vector.vec_from_cartesian 0.003 0.);
    ]
    []
    (Phys.Environment.make 500 500)

let ex2 =
  Phys.State.make
    [
      Phys.Object.make 0. (-0.4)
        (Phys.Object.create_rect 0.2 0.2)
        1.
        (Physmath.Vector.vec_from_cartesian 0.0 0.);
      Phys.Object.make 0. 0.4
        (Phys.Object.create_rect 0.2 0.2)
        1.
        (Physmath.Vector.vec_from_cartesian 0.003 0.);
    ]
    []
    (Phys.Environment.make 500 500)

let ex3 =
  Phys.State.make
    [
      Phys.Object.make 0. 0.
        (Phys.Object.create_rect 0.5 0.5)
        1.
        (Physmath.Vector.vec_from_cartesian 0.00000000001 0.);
    ]
    []
    (Phys.Environment.make 500 500)

(*let _ = Interface.main ex1*)

let _ = Interface.main ex3