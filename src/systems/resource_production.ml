

let run entity_templates gs (time_passed : float) =
  let tree = Quadtree.create {x=0.; y=0.} 32.0 in
  
  let unit_factories = Gamestate.unit_factories gs in

  let f ~key ~data tree =
    let point_mass = Gamestate.point_mass gs key in
    Quadtree.add tree key point_mass.position
  in

  let tree = Core.Hashtbl.fold unit_factories ~init:tree ~f in

  let open Gamestate in

  let gold_mine_ids = Core.Hashtbl.keys gs.resources in

  let f id =
    try 
      let open Component in
      let open Component.Point_mass in
      let pm = Gamestate.point_mass gs id in
      let nearby_bases = Quadtree.within tree pm.position 1. in

      ignore @@ Lwt_io.printf "%d bases in the vicinity of gold mine %d (%.1f|%.1f)\n"
                              (Core.List.length nearby_bases)
                              id
                              pm.position.x
                              pm.position.y;

      let number_of_bases = Core.List.length nearby_bases in
      
      let amount_per_s_per_player = 0.1 in

      let total_amount = amount_per_s_per_player *. (Core.Float.of_int number_of_bases) in

      let open Component.Resource in

      let f = function
        | Some { amount = current_amount } ->
           if current_amount < total_amount then (
             Lwt_io.printf "gold mine %d is running out gold\n" id;
             let per_player = current_amount /. (Core.Float.of_int number_of_bases) in
             Lwt_io.printf "giving every player %.1f gold\n" per_player;
             Some { amount = 0. }
           ) else (
             let per_player = amount_per_s_per_player *. time_passed in
             Lwt_io.printf "giving every player %.1f gold\n" per_player;

             let player_ids = Core.List.map nearby_bases ~f:(fun (base_id, _) ->
                                                             Gamestate.ownership gs base_id)
             in

             let bump_funds player_id =
               let open Player in
               Core.Hashtbl.change gs.players player_id
                           ~f:(function
                          | Some player ->
                             let funds = player.funds +. per_player in
                             Some {name=player.name; funds }
                          | None ->
                             Lwt_io.printf "missing player\n";
                             None
                  )
             in

             Core.List.iter player_ids ~f:bump_funds;

             Some { amount = current_amount -. total_amount }
           )
        | None ->
           ignore @@ Lwt_io.printf "missing resource component\n";
           None
      in
  
      Core.Hashtbl.change gs.resources id ~f

    with
    | Not_found -> ignore @@ Lwt_io.printf "not found\n"
    | _ -> ignore @@ Lwt_io.printf "error\n"

  in

  Core.List.iter gold_mine_ids ~f;

  gs
