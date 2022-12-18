open OUnit2
open QCheck

(* We used a combination of GlassBox and BlackBox. Mathmatical functions need
   both becuase on one hand we can cross referance our output to the results
   generated by other models we find online. However if you look at our
   implemntation, we need to allow for small variations in floating point error.
   For vectors we use black box to check identites known in vector arithmetic.
   For the math inside vibrations, we can check wether certain properties hold,
   like damped systems go to 0 as time goes to inf and the rest can be
   authenticated by manually checking the formulas. By testing the underlying
   math of the physic equations, then the application of those equations to
   physics problems must also be true.*)

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           Vector_test.vector_tests;
           Vector_test.qcheck_tests;
           State_test.state_tests;
           Vibrations_test.test;
         ]

let _ = run_test_tt_main suite
