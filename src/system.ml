


let movement gs time_passed : Gamestate.t =
  let now = Util.get_timestamp () in
  let duration = 5_000_000_000 in
  let f ~key ~data =
    ignore @@ Lwt_io.printf "%d units\n" (Component.Unit_factory.units_in_queue data);
    match Component.Unit_factory.peek data with
    | None -> ()
    | Some first ->
       let now = Util.get_timestamp () in
       let duration = 5_000_000_000 in
       if first + duration < now then (
         Component.Unit_factory.remove_first data;
         ignore @@ Lwt_io.printf "new unit\n"
       ) else (
         ignore @@ Lwt_io.printf "not yet ready\n"
       )
  in
  Core.Hashtbl.iteri (Gamestate.unit_factories gs) ~f;
  gs

let unit_production gs time_passed = gs

let commands gs time_passed = gs

(** Apply all systems and return the resulting game state *)
let run gs time_passed : Gamestate.t =
  let systems = [| unit_production; commands; movement |] in

  let f gs system = system gs time_passed in

  Core.Array.fold systems ~init:gs ~f
  
