open OUnit2
open QCheck
open Phys.Object
open Phys.State

let alec_tests =
  [
    ( "Check_collisions helper test using one circle obj list" >:: fun _ ->
      assert_equal 0.0
        ( Phys.State.make
            [
              Phys.Object.make 0.0 0.0
                (Phys.Object.create_circle 0.2)
                1.
                (Physmath.Vector.vec_from_cartesian 1.0 0.0);
            ]
            []
            (Phys.Environment.make 100 100)
        |> Phys.State.objs
        |> fun lst -> List.nth lst 0 |> Phys.Object.xpos )
        ~printer:string_of_float );
    ( "Check_collisions helper test using one circle obj list" >:: fun _ ->
      assert_equal 0.25
        ( Phys.State.make
            [
              Phys.Object.make 0.0 0.0
                (Phys.Object.create_circle 0.2)
                1.
                (Physmath.Vector.vec_from_cartesian 1.0 0.0);
            ]
            []
            (Phys.Environment.make 100 100)
        |> step_time |> Phys.State.objs
        |> fun lst -> List.nth lst 0 |> Phys.Object.xpos )
        ~printer:string_of_float );
    ( "Check_collisions helper test using one circle obj list" >:: fun _ ->
      assert_equal 0.5
        ( Phys.State.make
            [
              Phys.Object.make 0.0 0.0
                (Phys.Object.create_circle 0.2)
                1.
                (Physmath.Vector.vec_from_cartesian 1.0 0.0);
            ]
            []
            (Phys.Environment.make 100 100)
        |> step_time |> step_time |> Phys.State.objs
        |> fun lst -> List.nth lst 0 |> Phys.Object.xpos )
        ~printer:string_of_float );
    ( "Check_collisions helper test using one circle obj list" >:: fun _ ->
      assert_equal (-0.25)
        ( Phys.State.make
            [
              Phys.Object.make 0.0 0.0
                (Phys.Object.create_circle 0.2)
                1.
                (Physmath.Vector.vec_from_cartesian (-1.0) 0.0);
            ]
            []
            (Phys.Environment.make 100 100)
        |> step_time |> Phys.State.objs
        |> fun lst -> List.nth lst 0 |> Phys.Object.xpos )
        ~printer:string_of_float );
  ]