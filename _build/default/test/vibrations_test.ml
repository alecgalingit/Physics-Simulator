open OUnit2
open Phys.Oscillations

let zero_h_unforced_undamped =
  y_t 1000000000000000000000. (make 10. 0. 1.5 zero 0. 0.)

let zero_h_unforced_damped =
  y_t 1000000000000000000000. (make 10. 1. 1.5 zero 0. 0.)

let ic_h_unforced_undamped =
  y_t 1000000000000000000000. (make 10. 0. 1.5 zero 1. 0.)

let ic_h_unforced_damped =
  y_t 1000000000000000000000. (make 10. 1. 1.5 zero 1. 0.)

let within_range x_given x_expected =
  let lower = x_expected -. 0.1 in
  let upper = x_expected +. 0.1 in
  lower <= x_given || x_given >= upper

let vibe_test name given expected =
  name >:: fun _ -> assert_equal true (within_range given expected)

let test =
  [
    vibe_test "zero_h_unforced_undamped" zero_h_unforced_undamped 0.;
    vibe_test "zero_h_unforced_damped" zero_h_unforced_damped 0.;
    vibe_test "ic_h_unforced_undamped" ic_h_unforced_undamped 0.;
    vibe_test "ic_h_unforced_damped" ic_h_unforced_damped 0.;
  ]