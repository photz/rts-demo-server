(** Updates an entity that been ordered to go somewhere *)
let go_to entity_id vec gs time_passed=

  let open Gamestate in
  let open Component.Command in
  let open Vec in

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

(** Updates an entity that received a command to attack *)
let attack entity_id target gs time_passed =

  let open Gamestate in
  let open Component.Command in


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
       let now = Util.get_timestamp () in

       if armed.last_shot + armed.reload < now then (
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

         let halt_entity = function
           | Some point_mass ->
              Some (Component.Point_mass.halt point_mass)
           | None ->
              None
         in

         Core.Hashtbl.change gs.point_masses entity_id ~f:halt_entity;

         Core.Hashtbl.change gs.armed entity_id (fun _ ->
                               Some {damage = armed.damage;
                                     min_dist = armed.min_dist;
                                     reload = armed.reload;
                                     last_shot = now});

         ignore @@ Lwt_io.printf "unit %d can attack %d\n"
                                 entity_id target
       ) else (
         ignore @@ Lwt_io.printf "entity %d needs to wait before it can shoot again";
       )

    | false ->
       ignore @@ Lwt_io.printf "unit %d is too far away from %d to attack\n"
                               entity_id target;
       let xd = target_point_mass.position.x -. entity_point_mass.position.x
       in
       let yd = target_point_mass.position.y -. entity_point_mass.position.y
       in

       let len = Core.sqrt ((xd *. xd) +. (yd *. yd)) in

       let xd = xd /. len in
       let yd = yd /. len in

       let entity_point_mass =
         Component.Point_mass.update_velocity entity_point_mass
                                              {x = xd; y = yd}
       in

       Core.Hashtbl.change gs.point_masses entity_id ~f:(fun _ ->
                             Some entity_point_mass)
       
  end

(** System responsible for turning commands into actions *)
let run entity_templates gs time_passed =

  let open Gamestate in
  let open Component.Command in
  
  let update_entity entity_id =
    match Core.Hashtbl.find gs.commands entity_id with
    | Some(GoTo dest) -> go_to entity_id dest gs time_passed

    | Some (Idle) ->
       ignore @@ Lwt_io.printf "unit %d is doing nothing at all\n"
                               entity_id

    | Some(Attack target) ->
       attack entity_id target gs time_passed

    | _ -> ()
  in

  let entity_ids = Core.Hashtbl.keys gs.commands in

  Core.List.iter entity_ids ~f:update_entity;
  gs

