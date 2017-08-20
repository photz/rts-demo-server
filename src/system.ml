


let unit_production gs time_passed : Gamestate.t =
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
         let unit_pos = {x=pm.position.x -. 0.5; y=pm.position.y} in
         Entity.create_unit gs ~pos:unit_pos ~player:ownership;
         ignore @@ Lwt_io.printf "new unit\n"
       ) else (
         ignore @@ Lwt_io.printf "not yet ready\n"
       )
  in
  Core.Hashtbl.iteri (Gamestate.unit_factories gs) ~f;
  gs


let movement gs time_passed =
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

(** System responsible for turning commands into actions *)
let commands gs time_passed =

  let open Gamestate in
  let open Component.Command in
  
  let update_entity entity_id =
    match Core.Hashtbl.find gs.commands entity_id with
    | Some(GoTo vec) ->

       let pm = Gamestate.point_mass gs entity_id in

       let x = pm.position.x -. vec.x in
       let y = pm.position.y -. vec.y in

       let distance = sqrt ((x *. x) +. (y *. y)) in

       let x = x /. distance in
       let y = y /. distance in

       if distance < 0.1 then
         begin
           Core.Hashtbl.change gs.commands entity_id (fun _ ->
                                 Some Idle);
           Core.Hashtbl.change gs.point_masses entity_id (fun _ ->
                                 Some (Component.Point_mass.halt pm))
         end
       else
         begin
           ignore @@ Lwt_io.printf "unit %d was sent to %f,%f\n"
                                   entity_id
                                   vec.x
                                   vec.y;

           let open Component.Point_mass in

           let new_pm = update_velocity pm {x=(-1.0) *. x; y=(-1.0) *. y} in

           Core.Hashtbl.change gs.point_masses entity_id (fun _ ->
                                 Some new_pm)

         end
    | Some (Idle) ->
       ignore @@ Lwt_io.printf "unit %d is doing nothing at all\n"
                               entity_id

    | Some(Attack target) ->
       ignore @@ Lwt_io.printf "entity %d is attacking %d\n"
                               entity_id
                               target;

       let entity_point_mass = Gamestate.point_mass gs entity_id in
       let target_point_mass = Gamestate.point_mass gs target in

       let distance = Component.Point_mass.distance entity_point_mass
                                                    target_point_mass
       in

       let armed = Gamestate.armed gs entity_id in

       let open Component.Armed in
       let open Component.Health in

       let within_distance = distance < armed.min_dist in

       begin
         match within_distance with

         | true ->
            let {hp; max_hp} = Core.Hashtbl.find_exn gs.health target in

            let hp = hp -. armed.damage in

            ignore @@ Lwt_io.printf "health of %d down to %f\n"
                                    target hp;

            if hp <= 0.0 then (
              Gamestate.remove_entity gs target;
              Core.Hashtbl.change gs.commands entity_id (fun _ ->
                                    Some Idle)
            ) else (
              Core.Hashtbl.change gs.health target (fun _ ->
                                    Some {hp; max_hp})
            );

            ignore @@ Lwt_io.printf "unit %d can attack %d\n"
                                    entity_id target
         | false ->
            ignore @@ Lwt_io.printf "unit %d is too far away from %d to attack\n"
                                    entity_id target
       end
    | _ -> ()
  in

  let entity_ids = Core.Hashtbl.keys gs.commands in

  Core.List.iter entity_ids ~f:update_entity;

  gs


(** Apply all systems and return the resulting game state *)
let run gs time_passed : Gamestate.t =
  let systems = [| unit_production; commands; movement |] in

  let f gs system = system gs time_passed in

  Core.Array.fold systems ~init:gs ~f
  
