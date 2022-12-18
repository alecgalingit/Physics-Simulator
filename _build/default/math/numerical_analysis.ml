type yt = float * float
type yy't = float * float * float

let first_order yp t_0 y_0 : yt =
  let h = Constants.h in
  let k1 = h *. yp t_0 y_0 in
  let k2 = h *. yp (t_0 +. (h /. 2.)) (y_0 +. (k1 /. 2.)) in
  let k3 = h *. yp (t_0 +. (h /. 2.)) (y_0 +. (k2 /. 2.)) in
  let k4 = h *. yp (t_0 +. h) (y_0 +. k3) in
  let t_1 = t_0 +. h in
  let y_1 = y_0 +. ((k1 +. (2. *. k2) +. (2. *. k3) +. k4) /. 6.) in
  (t_1, y_1)

let second_order a t0 x0 v0 =
  let open Constants in
  let dv1 = h *. a t0 x0 v0 in
  (* dv1 = h F(t,x,v) *)
  let dx1 = h *. v0 in

  (* dx1 = h * v *)
  let dv2 = h *. a (t0 +. (h /. 2.)) (x0 +. (dx1 /. 2.)) (v0 +. (dv1 /. 2.)) in
  (* dv2 = h F(t + h/2, x + dx1/2, v + dv1/2) *)
  let dx2 = h *. (v0 +. (dv1 /. 2.)) in

  (* dx2 = h (v + dv1/2) *)
  let dv3 = h *. a (t0 +. (h /. 2.)) (x0 +. (dx2 /. 2.)) (v0 +. (dv1 /. 2.)) in
  (* dv2 = h F(t + h/2, x + dx2/2, v + dv1/2) *)
  let dx3 = h *. (v0 +. (dv2 /. 2.)) in

  (* dx2 = h (v + dv2/2) *)
  let dv4 = h *. a (t0 +. h) (x0 +. dx3) (v0 +. dv1) in
  (* dv4 = h F(t + h, x + dx3 + v + dv1) *)
  let dx4 = h *. (v0 +. dv3) in

  (* dx2 = h (v + dv3) *)
  let dx = (dx1 +. (2. *. dx2) +. (2. *. dx3) +. dx4) /. 6. in
  let dv = (dv1 +. (2. *. dv2) +. (2. *. dv3) +. dv4) /. 6. in

  let t1 = t0 +. h in
  let x1 = x0 +. dx in
  let v1 = v0 +. dv in
  (t1, x1, v1)

let time_first = function
  | t, _ -> t

let y_first = function
  | _, y -> y

let time_second = function
  | t, _, _ -> t

let y_second = function
  | _, y, _ -> y

let y'_second = function
  | _, _, y' -> y'

let f_ty (f : float) t y = f
let f_tyy' (f : float) t y y' = f
