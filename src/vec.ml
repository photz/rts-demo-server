type vec2 = { x: float; y: float }

(** Determines the distance between two points in 2d *)                   
let distance a b =
  let x_d = a.x -. b.x in
  let y_d = a.y -. b.y in
  Core.Float.sqrt(x_d *. x_d +. y_d *. y_d)

