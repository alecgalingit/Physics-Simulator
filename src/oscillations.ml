let gravity = 9.8

type system = {
  spring : spring;
  mass : mass;
  damper : damper;
  is_damped : bool;
}

and spring = {
  dl : float;
  spring_coef : float;
}

and mass = {
  mass : float;
  x_pos : float;
  y_pos : float;
}

and damper = { damper_coef : float }

type forcing_function =
  | Zero
  | Gravity
  | Sin of float * float
  | Cos of float * float

type gov_eqn = {
  system : system;
  forcing_function : forcing_function;
  is_forced : bool;
}

type y =
  | Undamped of float * float * float
  | Damped of float * float * float * float
  | DampedGravity of y * float
  | DampedSinInput of y * float * float * float
  | DampedCosInput of y * float * float
  | UndampedGravity of y * float
  | UndampedCosInput of y * float * float
  | UndampedSinInput of y * float * float * float

let find_yc y =
  match y with
  | Undamped (_, _, _) | Damped (_, _, _, _) -> y
  | DampedGravity (yc, _) -> yc
  | DampedSinInput (yc, _, _, _) -> yc
  | DampedCosInput (yc, _, _) -> yc
  | UndampedGravity (yc, _) -> yc
  | UndampedSinInput (yc, _, _, _) -> yc
  | UndampedCosInput (yc, _, _) -> yc

let make_system sp ma da =
  let is_damped = da = 0. in
  {
    spring = { dl = 0.; spring_coef = sp };
    mass = { mass = ma; x_pos = 0.; y_pos = 0. };
    damper = { damper_coef = da };
    is_damped;
  }

let make_trig amp freq sin = if sin then Sin (amp, freq) else Cos (amp, freq)

(** [make_force_eqn] *)
let make_gov_eqn sys f =
  match f with
  | Zero -> { system = sys; forcing_function = f; is_forced = false }
  | Gravity | Sin (_, _) | Cos (_, _) ->
      { system = sys; forcing_function = f; is_forced = true }

(** [solve_free sys y y'] solves the free second order ODE *)
let solve_free sys y y' =
  let nat_freq = sys.spring.spring_coef /. sys.mass.mass |> Float.sqrt in
  if sys.is_damped then
    let damp_ratio =
      sys.damper.damper_coef
      /. ((sys.spring.spring_coef *. sys.mass.mass |> Float.sqrt) *. 2.)
    in
    let c1 = y in
    let c2 = (y' +. (damp_ratio *. nat_freq *. c1)) /. nat_freq in
    Damped (damp_ratio, nat_freq, c1, c2)
  else
    let c1 = y in
    let c2 = y' /. nat_freq in
    Undamped (nat_freq, c1, c2)

(** [solve_forced sys force y y'] solves the forced second order ODE *)
let solve_forced sys force y y' =
  (* phase shift *)
  let nat_freq = sys.spring.spring_coef /. sys.mass.mass |> Float.sqrt in
  if sys.is_damped then
    let damp_ratio =
      sys.damper.damper_coef
      /. ((sys.spring.spring_coef *. sys.mass.mass |> Float.sqrt) *. 2.)
    in
    match force with
    | Zero -> failwith "then its not forced"
    | Gravity ->
        let c1 = y +. (gravity *. sys.mass.mass /. sys.spring.spring_coef) in
        let c2 = (y' +. (damp_ratio *. nat_freq *. c1)) /. nat_freq in
        DampedGravity
          ( Damped (damp_ratio, nat_freq, c1, c2),
            gravity *. sys.mass.mass /. sys.spring.spring_coef )
    | Cos (input_amp, input_freq) ->
        (* fix here *)
        let r =
          input_amp
          /. (sys.mass.mass *. sys.mass.mass
              *. ((nat_freq *. nat_freq) -. (input_freq *. input_freq))
             +. sys.damper.damper_coef *. sys.damper.damper_coef
                *. (input_freq *. input_freq))
        in
        let c1 = y -. r in
        let c2 = (y' +. (damp_ratio *. nat_freq *. c1)) /. nat_freq in
        DampedCosInput (Damped (damp_ratio, nat_freq, c1, c2), r, input_freq)
    | Sin (input_amp, input_freq) ->
        let r =
          input_amp
          /. (sys.mass.mass *. sys.mass.mass
              *. ((nat_freq *. nat_freq) -. (input_freq *. input_freq))
             +. sys.damper.damper_coef *. sys.damper.damper_coef
                *. (input_freq *. input_freq))
        in
        let c1 = y -. r in
        let c2 = (y' +. (damp_ratio *. nat_freq *. c1)) /. nat_freq in
        DampedSinInput
          (Damped (damp_ratio, nat_freq, c1, c2), r, input_freq, Float.pi /. 2.)
  else
    match force with
    | Zero -> failwith "then its not forced"
    | Gravity ->
        let c1 = y +. (gravity *. sys.mass.mass /. sys.spring.spring_coef) in
        let c2 = y' /. nat_freq in
        UndampedGravity
          ( Undamped (nat_freq, c1, c2),
            gravity *. sys.mass.mass /. sys.spring.spring_coef )
    | Cos (input_amp, input_freq) ->
        let r =
          input_amp
          /. (sys.mass.mass *. sys.mass.mass
             *. ((nat_freq *. nat_freq) -. (input_freq *. input_freq))
             *. (input_freq *. input_freq))
        in
        let c1 = y -. r in
        let c2 = y' /. nat_freq in
        UndampedCosInput (Undamped (nat_freq, c1, c2), r, input_freq)
    | Sin (input_amp, input_freq) ->
        let r =
          input_amp
          /. (sys.mass.mass *. sys.mass.mass
             *. ((nat_freq *. nat_freq) -. (input_freq *. input_freq))
             *. (input_freq *. input_freq))
        in
        let c1 = y -. r in
        let c2 = y' /. nat_freq in
        UndampedSinInput
          (Undamped (nat_freq, c1, c2), r, input_freq, Float.pi /. 2.)

(** [solve gov y y'] solves the ODE force with initial condtions y y' *)
let solve gov y y' =
  match gov with
  | { system; forcing_function; is_forced = true } ->
      solve_forced system forcing_function y y'
  | { system; forcing_function; is_forced = false } -> solve_free system y y'

(** [y_t y t] is the position of a spring that is forceerend by eqn y at time t *)
let rec y_t t y =
  match y with
  | Undamped (nat_freq, c1, c2) ->
      (c1 *. Float.cos (nat_freq *. t)) +. (c2 *. Float.sin (nat_freq *. t))
  | Damped (damp, nat_freq, c1, c2) ->
      Float.exp (-1. *. damp *. nat_freq *. t)
      *. ((c1 *. Float.cos (nat_freq *. t)) +. (c2 *. Float.sin (nat_freq *. t)))
  | DampedGravity (yc, grav_shift) ->
      let yc = find_yc yc in
      let yc_t = y_t t yc in
      yc_t -. grav_shift
  | DampedSinInput (yc, r, input_freq, phase) ->
      let yc = find_yc yc in
      let yc_t = y_t t yc in
      yc_t +. (r *. Float.cos ((input_freq *. t) +. phase))
  | DampedCosInput (yc, r, input_freq) ->
      let yc = find_yc yc in
      let yc_t = y_t t yc in
      yc_t +. (r *. Float.cos (input_freq *. t))
  | UndampedGravity (yc, grav_shift) ->
      let yc = find_yc yc in
      let yc_t = y_t t yc in
      yc_t -. grav_shift
  | UndampedSinInput (yc, r, input_freq, phase) ->
      let yc = find_yc yc in
      let yc_t = y_t t yc in
      yc_t +. (r *. Float.cos ((input_freq *. t) +. phase))
  | UndampedCosInput (yc, r, input_freq) ->
      let yc = find_yc yc in
      let yc_t = y_t t yc in
      yc_t +. (r *. Float.cos (input_freq *. t))

(** [make m c k] is the eqn without the y(0) y'(0) applied *)
let make m c k force y y' =
  match c = 0. with
  | true ->
      let sys = make_system k m c in
      let gov = make_gov_eqn sys force in
      solve gov y y'
  | false ->
      let sys = make_system k m c in
      let gov = make_gov_eqn sys force in
      solve gov y y'

let zero = Zero
let grav = Gravity
let sin amp freq = Sin (amp, freq)
let cos amp freq = Cos (amp, freq)
