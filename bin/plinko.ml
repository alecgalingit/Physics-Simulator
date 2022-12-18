open ANSITerminal

let state =
  let floor =
    Phys.Object.make 0. (-5.) Phys.Object.floor 10000000.
      (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679)
  in
  let unit_ball =
    Phys.Object.make 50. 100. Phys.Object.unit_ball 10.
      (Physmath.Vector.vec_from_polar 0. (-1.57079632679))
  in
  let game_ball =
    Phys.Object.add_force unit_ball
      Physmath.Vector.(Physmath.Constants.gravity *> Phys.Object.mass unit_ball)
  in
  let make_peg_row ypos =
    if int_of_float (Float.round ypos +. 0.1) / 10 mod 2 = 0 then
      [
        Phys.Object.make 25. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 30. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 35. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 40. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 45. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 50. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 55. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 60. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 65. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 70. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 75. ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
      ]
    else
      [
        Phys.Object.make 27.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 32.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 37.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 42.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 47.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 52.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 57.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 62.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 67.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
        Phys.Object.make 72.5 ypos Phys.Object.plinko_peg 10000000.
          (Physmath.Vector.vec_from_polar 0.0000001 1.57079632679);
      ]
  in
  let peg_board =
    List.flatten
      [
        make_peg_row 90.;
        make_peg_row 80.;
        make_peg_row 70.;
        make_peg_row 60.;
        make_peg_row 50.;
        make_peg_row 40.;
        make_peg_row 30.;
        make_peg_row 20.;
        make_peg_row 10.;
      ]
  in
  let env = Phys.Environment.make 100 100 in
  Phys.State.make (List.flatten [ [ game_ball; floor ]; peg_board ]) [] env

let loop (st : Phys.State.state) = Phys.State.step_time st

let rec make_state_list ?(time = Physmath.Constants.runtime)
    (st : Phys.State.state) list =
  if Phys.State.time st >= time then list
  else make_state_list ~time (Phys.State.step_time st) (st :: list)

let rec print_loop = function
  | [] -> ANSITerminal.print_string [ ANSITerminal.red ] "End of Simulation"
  | head :: tail ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        (Phys.State.state_to_string head);
      print_loop tail

let main () = make_state_list state [] |> List.rev |> print_loop

(* execute the phys engine. *)
let () = main ()