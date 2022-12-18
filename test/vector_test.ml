open OUnit2
open QCheck

let mag_scalar_test (name : string) (vector : Physmath.Vector.vector)
    (scalar : float) (exp_out : float) : test =
  let open Physmath.Vector in
  name >:: fun _ ->
  assert_equal (mag (vector *> scalar)) exp_out ~printer:string_of_float

let theta_scalar_test (name : string) (vector : Physmath.Vector.vector)
    (scalar : float) (exp_out : float) : test =
  let open Physmath.Vector in
  name >:: fun _ ->
  assert_equal (theta (vector *> scalar)) exp_out ~printer:string_of_float

let mag_test vector exp_mag : test =
  let open Physmath.Vector in
  "("
  ^ (vector |> x_of_vector |> string_of_float)
  ^ ", "
  ^ (vector |> y_of_vector |> string_of_float)
  ^ ")"
  >:: fun _ -> assert_equal (mag vector) exp_mag

let cart_test (name : string) (x : float) (y : float) (exp_mag : float) : test =
  let open Physmath.Vector in
  name >:: fun _ ->
  assert_equal (vec_from_cartesian x y |> mag) exp_mag ~printer:string_of_float

let polar_test (name : string) (r : float) (t : float) f (exp : float) : test =
  let open Physmath.Vector in
  name >:: fun _ ->
  assert_equal (vec_from_polar r t |> f) exp ~printer:string_of_float

let x_y =
  let open Physmath.Vector in
  x_unit_vector +> y_unit_vector

let z_y =
  let open Physmath.Vector in
  zero_vector +> y_unit_vector

let z_z =
  let open Physmath.Vector in
  zero_vector +> zero_vector

let z_z_z =
  let open Physmath.Vector in
  zero_vector +> zero_vector +> zero_vector

let x_y10 =
  let open Physmath.Vector in
  (x_unit_vector *> 10.) +> (y_unit_vector *> 10.)

let rec make_vec_list ?(count = 0) vec list =
  match count with
  | 100 -> list
  | _ -> make_vec_list vec (vec :: list) ~count:(count + 1)

let x_vec_list_100 = make_vec_list Physmath.Vector.x_unit_vector []
let y_vec_list_100 = make_vec_list Physmath.Vector.y_unit_vector []
let zero_vec_list_100 = make_vec_list Physmath.Vector.zero_vector []
let vec_list_100 = make_vec_list Physmath.Vector.(vec_from_cartesian 1. 1.) []

let net_vec_mag name vec_lst exp f =
  name >:: fun _ ->
  assert_equal
    Physmath.Vector.(vec_lst |> net_vec |> f)
    exp ~cmp:Float.equal ~printer:string_of_float

let zero_mult_scalar scalar =
  let open Physmath.Vector in
  mag (zero_vector *> scalar) = 0.

let x_unit_vector_scalar scalar =
  let open Physmath.Vector in
  let x' = x_unit_vector *> scalar in
  mag x' = scalar && theta x' = 0.

let y_unit_vector_scalar scalar =
  let open Physmath.Vector in
  let y' = y_unit_vector *> scalar in
  mag y' = scalar && theta y' = Float.pi /. 2.

let zero_add x =
  let open Physmath.Vector in
  let v' = vec_from_cartesian x (x *. 3.1) in
  let z' = zero_vector +> v' in
  x_of_vector z' = x_of_vector v' && y_of_vector z' = y_of_vector v'

let zero_mag_zero =
  QCheck.Test.make ~count:1000
    ~name:
      "Magnitude of Zero vector is always zero regardless of scalar \
       multiplication"
    QCheck.float zero_mult_scalar

let x_unit_scalar =
  QCheck.Test.make ~count:1000
    ~name:
      "Magnitude of X Unit vector is always equal to the scalar and the theta \
       is always 0."
    QCheck.float x_unit_vector_scalar

let y_unit_scalar =
  QCheck.Test.make ~count:1000
    ~name:
      "Magnitude of Y Unit vector is always equal to the scalar and the theta \
       is always pi/2"
    QCheck.float y_unit_vector_scalar

let zero_add_test =
  QCheck.Test.make ~count:1000
    ~name:"Adding any vector to the zero vector results in that vector."
    QCheck.float zero_add

let dot_test name x x' resultant =
  name >:: fun _ ->
  assert_equal
    Physmath.Vector.(dot x x')
    resultant ~printer:string_of_float ~cmp:Float.equal

let qcheck_tests =
  List.map QCheck_ounit.to_ounit2_test
    [ zero_mag_zero; x_unit_scalar; y_unit_scalar; zero_add_test ]

let vector_tests =
  [
    mag_scalar_test "Zero vector" Physmath.Vector.zero_vector 1. 0.;
    mag_scalar_test "Unit * 2.5 = 2.5" Physmath.Vector.x_unit_vector 2.5 2.5;
    theta_scalar_test "angle Unit * 2.5 = 0" Physmath.Vector.x_unit_vector 2.5
      0.;
    cart_test "[cart_test] (0,0)" 0. 0. 0.;
    cart_test "[cart_test] (1,0)" 1. 0. 1.;
    cart_test "[cart_test] (0,1)" 0. 1. 1.;
    cart_test "[cart_test] (3,4)" 3. 4. 5.;
    cart_test "[cart_test] (-3,-4)" (-3.) (-4.) 5.;
    cart_test "[cart_test] (-3,4)" (-3.) 4. 5.;
    cart_test "[cart_test] (3,-4)" 3. (-4.) 5.;
    polar_test "[polar_test] (0,0)" 0. 0. Physmath.Vector.mag 0.;
    polar_test "[polar_test] (0,0)" 0. 0. Physmath.Vector.theta 0.;
    polar_test "[polar_test] (1,1)" 1. 1. Physmath.Vector.mag 1.;
    polar_test "[polar_test] (1,1)" 1. 1. Physmath.Vector.theta 1.;
    polar_test "[polar_test] (1,1)" 10. 1. Physmath.Vector.mag 10.;
    polar_test "[polar_test] (1,1)" 1. 1. Physmath.Vector.theta 1.;
    polar_test "[polar_test] (1,1)" 10. 1. Physmath.Vector.mag 10.;
    polar_test "[polar_test] (1,1)" 1. (-1.) Physmath.Vector.theta (-1.);
    mag_test x_y (Float.sqrt 2.);
    mag_test z_y 1.;
    mag_test x_y10 (Float.sqrt (10. *. 10. *. 2.));
    mag_test (Physmath.Vector.net_vec x_vec_list_100) 100.;
    mag_test (Physmath.Vector.net_vec y_vec_list_100) 100.;
    mag_test z_z 0.;
    Physmath.Vector.(
      dot_test "[dot_test] <0,0> . <0,0> = 0" zero_vector zero_vector 0.);
    Physmath.Vector.(
      dot_test "[dot_test] <0,0> . <1,0> = 0" zero_vector x_unit_vector 0.);
    Physmath.Vector.(
      dot_test "[dot_test] <0,0> . <0,1> = 0" zero_vector y_unit_vector 0.);
    (* Phys.Vector.( dot_test "[dot_test] <1,0> . <0,1> = 0" x_unit_vector
       y_unit_vector Float.zero); *)
    Physmath.Vector.(
      dot_test "[dot_test] <1,1> . <0,1> = 1" (vec_from_cartesian 1. 1.)
        y_unit_vector 1.);
    (* Phys.Vector.( dot_test "[dot_test] <1,1> . <1,1> = 2" (vec_from_cartesian
       1. 1.) (vec_from_cartesian 1. 1.) 2.0); *)
    Physmath.Vector.(
      dot_test "[dot_test] <2,1> . <2,1> = 5" (vec_from_cartesian 2. 1.)
        (vec_from_cartesian 2. 1.) 5.);
    net_vec_mag "[net_vec_mag] mag zero_vec_list = 0" zero_vec_list_100 0.
      Physmath.Vector.mag;
    net_vec_mag "[net_vec_mag] mag x_vec_list_100 = 100" x_vec_list_100 100.
      Physmath.Vector.mag;
    net_vec_mag "[net_vec_mag] mag y_vec_list_100 = 100" y_vec_list_100 100.
      Physmath.Vector.mag;
    net_vec_mag "[net_vec_mag] theta x_vec_list_100 = 0" x_vec_list_100 0.
      Physmath.Vector.theta;
    net_vec_mag "[net_vec_mag] theta y_vec_list_100 = Pi/2" y_vec_list_100
      (Float.pi /. 2.) Physmath.Vector.theta;
  ]
