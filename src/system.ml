


let movement gs time_passed =
  let now = Util.get_timestamp () in
  let duration = 5 * 1000 * 1000 * 1000 in
  let f ~key ~data =
    ignore @@ Lwt_io.printf "%d units" (Component.Unit_factory.units_in_queue data)
  in
  Core.Hashtbl.iteri (Gamestate.unit_factories gs) ~f


let unit_production (gs : Gamestate.t) (time_passed : float) = ()

let commands (gs : Gamestate.t) (time_passed : float) = ()


let run gs time_passed =
  let systems = [| unit_production; commands; movement |] in
  Core.Array.iter systems ~f:(fun x -> x gs time_passed)
  
