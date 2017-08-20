
let run gs time_passed : Gamestate.t =
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
         let open Gamestate in
         let pm = Core.Hashtbl.find_exn gs.point_masses key in
         let open Component in
         let ownership = Core.Hashtbl.find_exn gs.ownership key in
         let open Vec in
         let unit_pos = {x=pm.position.x -. 0.5; y=pm.position.y} in
         Entity.create_unit gs ~pos:unit_pos ~player:ownership;
         ignore @@ Lwt_io.printf "new unit\n"
       ) else (
         ignore @@ Lwt_io.printf "not yet ready\n"
       )
  in
  Core.Hashtbl.iteri (Gamestate.unit_factories gs) ~f;
  gs
