open ANSITerminal

let state =
  let floor =
    Phys.Object.make 0. 0. Phys.Object.floor 1000000000.
      (Physmath.Vector.vec_from_polar 0.000000001 1.57079632679)
  in
  let sq =
    Phys.Object.make 45. 5.5 Phys.Object.square 10.
      (Physmath.Vector.vec_from_polar 0. (-1.57079632679))
  in
  let sqf =
    Phys.Object.add_force sq
      Physmath.Vector.(Physmath.Constants.gravity *> Phys.Object.mass sq)
  in
  let env = Phys.Environment.make 100 100 in
  Phys.State.make [ sqf; floor ] [] env

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