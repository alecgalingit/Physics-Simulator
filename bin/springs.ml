open ANSITerminal

let und_free_displaced =
  Phys.Oscillations.make 3. 0. 1.5 Phys.Oscillations.zero 0.5 0.

let und_free_moving =
  Phys.Oscillations.make 5.5 0. 2.0 Phys.Oscillations.zero 0. 0.5

let und_free_still =
  Phys.Oscillations.make 100. 0. 20. Phys.Oscillations.zero 0. 0.

(* DAMPED FREE *)
let damp_free_displaced =
  Phys.Oscillations.make 3. 0.1 1.5 Phys.Oscillations.zero 0.5 0.

let damp_free_moving =
  Phys.Oscillations.make 5.5 1. 2.0 Phys.Oscillations.zero 0. 0.5

let damp_free_still =
  Phys.Oscillations.make 100. 5. 20. Phys.Oscillations.zero 0. 0.

(* UNDAMPED FORCED *)
let und_forced_displaced =
  Phys.Oscillations.make 3. 0. 1.5
    Phys.Oscillations.(sin 3. (Float.sqrt 0.5))
    0.5 0.

let und_forced_moving =
  Phys.Oscillations.make 5.5 0. 2.0 Phys.Oscillations.grav 0. 0.5

let und_forced_still =
  Phys.Oscillations.make 100. 0. 20. Phys.Oscillations.(cos 3. 0.5) 0. 0.

(* DAMPED FORCED *)
let damp_forced_displaced =
  Phys.Oscillations.make 3. 0.1 1.5
    Phys.Oscillations.(sin 3. (Float.sqrt 0.5))
    0.5 0.

let damp_forced_moving =
  Phys.Oscillations.make 5.5 2. 2.0 Phys.Oscillations.grav 0. 0.5

let damp_forced_still =
  Phys.Oscillations.make 100. 20. 20. Phys.Oscillations.(cos 3. 0.5) 0. 0.

let spring_list =
  [
    ( "Undamped Free Initial Displacement",
      Phys.Oscillations.y_t 0. und_free_displaced,
      und_free_displaced );
    ( "Undamped Free Initial Velocity",
      Phys.Oscillations.y_t 0. und_free_moving,
      und_free_moving );
    ( "Undamped Free Initial Still",
      Phys.Oscillations.y_t 0. und_free_still,
      und_free_still );
    ( "Undamped Forced Initial Displacement",
      Phys.Oscillations.y_t 0. und_forced_displaced,
      und_forced_displaced );
    ( "Undamped Forced Initial Velocity",
      Phys.Oscillations.y_t 0. und_forced_moving,
      und_forced_moving );
    ( "Undamped Forced Initial",
      Phys.Oscillations.y_t 0. und_forced_still,
      und_forced_still );
    ( "Damped Free Initial Displacement",
      Phys.Oscillations.y_t 0. damp_free_displaced,
      damp_free_displaced );
    ( "Damped Free Initial Velocity",
      Phys.Oscillations.y_t 0. damp_free_moving,
      damp_free_moving );
    ( "Damped Free Initial Still",
      Phys.Oscillations.y_t 0. damp_free_still,
      damp_free_still );
    ( "Damped Forced Initial Displacement",
      Phys.Oscillations.y_t 0. damp_forced_displaced,
      damp_forced_displaced );
    ( "Damped Forced Initial Velocity",
      Phys.Oscillations.y_t 0. damp_forced_moving,
      damp_forced_moving );
    ( "Damped Forced Initial Still",
      Phys.Oscillations.y_t 0. damp_forced_still,
      damp_forced_still );
  ]

let state2 =
  let env = Phys.Environment.make 100 100 in
  Phys.State.make [] spring_list env

let loop (st : Phys.State.state) = Phys.State.step_time st

let rec make_state_list ?(time = 10.) (st : Phys.State.state) list =
  if Phys.State.time st >= time then list
  else make_state_list ~time (Phys.State.step_time st) (st :: list)

let rec print_loop = function
  | [] -> ANSITerminal.print_string [ ANSITerminal.red ] "End of Simulation"
  | head :: tail ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        (Phys.State.state_to_string head);
      print_loop tail

let main () = make_state_list state2 [] |> List.rev |> print_loop

(* Execute the phys engine. *)
let () = main ()
