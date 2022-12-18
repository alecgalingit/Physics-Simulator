open OUnit2
open QCheck
open Phys.Object
open Phys.State

let env = Phys.Environment.make 100 100

let ballPos1 =
  Phys.Object.make 10. 40. Phys.Object.ball 10.
    (Physmath.Vector.vec_from_polar 40. (-1.57079632679))

let ballPos2 =
  Phys.Object.make 10. 0. Phys.Object.ball 10.
    (Physmath.Vector.vec_from_polar 40. (-1.57079632679))

let ballPos3 =
  Phys.Object.make 10. 30. Phys.Object.ball 10.
    (Physmath.Vector.vec_from_polar 40. (-1.57079632679))

let sqPos1 =
  Phys.Object.make 10. 40. Phys.Object.square 10.
    (Physmath.Vector.vec_from_polar 40. (-1.57079632679))

let sqPos2 =
  Phys.Object.make 10. 0. Phys.Object.square 10.
    (Physmath.Vector.vec_from_polar 40. (-1.57079632679))

let sqPos3 =
  Phys.Object.make 5. 35. Phys.Object.square 10.
    (Physmath.Vector.vec_from_polar 40. (-1.57079632679))

let circleSame = make [ ballPos1; ballPos1 ] [] env
let circleOverlap = make [ ballPos1; ballPos3 ] [] env
let circleNoOverlap = make [ ballPos1; ballPos2 ] [] env
let squareSame = make [ sqPos1; sqPos1 ] [] env
let squareOverlap = make [ sqPos1; sqPos3 ] [] env
let squareNoOverlap = make [ sqPos1; sqPos2 ] [] env
(* let multiOverlap = make [ ballPos2; sqPos2 ] [] env *)
(* let multiNoOverlap = make [ ballPos2; sqPos1 ] [] env *)

let collision_helper_testmaker (name : string) (obj1 : obj) (obj2 : obj)
    (helper : obj -> obj -> bool) (expectedoutput : bool) : test =
  let open Physmath.Vector in
  name >:: fun _ ->
  assert_equal expectedoutput (helper obj1 obj2) ~printer:string_of_bool

let collide_state_testmaker (name : string) (state : state)
    (expectedoutput : bool) : test =
  let open Physmath.Vector in
  name >:: fun _ ->
  assert_equal expectedoutput (return_collide state) ~printer:string_of_bool

let state_tests =
  [
    collision_helper_testmaker
      "testing two rectangles with two overlapping rectangles" sqPos1 sqPos3
      two_rect_overlap true;
    collision_helper_testmaker
      "testing two rectangles with two non overlapping rectangles" sqPos1 sqPos2
      two_rect_overlap false;
    collision_helper_testmaker
      "testing two circle helper with overlapping circles." ballPos1 ballPos3
      two_circle_overlap true;
    collision_helper_testmaker
      "testing two circle helper with non overlapping circles." ballPos1
      ballPos2 two_circle_overlap false;
    collision_helper_testmaker
      "testing two circle helper with same overlapping circles." ballPos1
      ballPos1 two_circle_overlap true;
    collision_helper_testmaker
      "testing one circle helper with overlapping square circle." ballPos2
      sqPos2 one_circle_overlap true;
    collision_helper_testmaker
      "testing one circle helper with non overlapping square circle." ballPos2
      sqPos1 one_circle_overlap false;
    (* collide_state_testmaker "testing collide matching through Two same
       circular objects should be \ in collision" circleSame true; *)
    collide_state_testmaker
      "testing collide matching through Two overlapping circular objects \
       should not be in collision"
      circleOverlap true;
    collide_state_testmaker
      "testing collide matching through Two non overlapping circular objects \
       should not be in collision"
      circleNoOverlap false;
    (* collide_state_testmaker "testing collide matching through Two same
       rectangular objects should be \ in collision" squareSame true; *)
    collide_state_testmaker
      "testing collide matching through Two overlapping rectangular objects \
       should be in collision"
      squareOverlap true;
    collide_state_testmaker
      "testing collide matching through Two non-overlapping rectangular \
       objects should not be in collision"
      squareNoOverlap false;
    (* collide_state_testmaker
      "testing collide matching through an object of each type, circle and \
       rectangle, which are overlapping should be in collision."
      multiOverlap true; *)
    collide_state_testmaker
      "testing collide matching through an object of each type, circle and \
       rectangle, which are not overlapping should not be in collision."
      squareSame false;
    ( "Check_collisions helper test using empty obj list" >:: fun _ ->
      assert_equal false (check_collisions [] []) ~printer:string_of_bool );
    ( "Check_collisions helper test using one circle obj list" >:: fun _ ->
      assert_equal false
        (check_collisions [ ballPos1 ] [ ballPos1 ])
        ~printer:string_of_bool )
    (*objects being checked against themselves!! current solution just to not
      have it be possible in implementation for two objects to be exactly the
      same. *);
  ]