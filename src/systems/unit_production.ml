let run entity_templates gs time_passed : Gamestate.t =
  let now = Util.get_timestamp () in
  let duration = 5_000_000_000 in
  let f gs key =
    let data = Gamestate.unit_factory gs key in
    match Component.Unit_factory.peek data with
    | None -> gs
    | Some template_id ->
       if data.began + duration < now then (
         let factory = Component.Unit_factory.remove_first data in

         let open Gamestate in

         Core.Hashtbl.change gs.unit_factories key ~f:(fun _ ->
                               Some factory);

         let pm = Core.Hashtbl.find_exn gs.point_masses key in

         let open Component in
         let ownership = Core.Hashtbl.find_exn gs.ownership key in
         let open Vec in
         let open Component.Point_mass in
         let unit_pos = {x=pm.position.x -. 3.0; y=pm.position.y-.3.0} in

         let pm = {position=unit_pos;
                   velocity={x=0.0; y=0.0};
                   orientation=Core.Random.float 360.} in

         let tpl = Core.Map.find_exn entity_templates template_id in

         ignore @@ Lwt_io.printf "new unit\n";

         Entity.Template.spawn ~point_mass:pm
                               ~ownership
                               gs
                               tpl
       ) else (
         ignore @@ Lwt_io.printf "not yet ready\n";
         gs
       )
  in

  let open Gamestate in
  let entity_ids = Core.Hashtbl.keys gs.unit_factories in
  Core.List.fold ~init:gs entity_ids ~f

