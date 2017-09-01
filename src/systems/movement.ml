

let run entity_templates gs time_passed =
  let point_masses = Gamestate.point_masses gs in

  let open Gamestate in

  let update_entity entity_id =
    let point_mass = Core.Hashtbl.find_exn gs.point_masses entity_id in
    let open Component in
    let new_point_mass = Point_mass.update_pos point_mass time_passed in
    Core.Hashtbl.change gs.point_masses entity_id (fun _ ->
                          Some new_point_mass);
    ignore @@ Lwt_io.printf "unit %d now at %f,%f\n"
                            entity_id
                            new_point_mass.position.x
                            new_point_mass.position.y
  in
  let entity_ids = Core.Hashtbl.keys gs.point_masses in

  Core.List.iter entity_ids update_entity;

  gs
