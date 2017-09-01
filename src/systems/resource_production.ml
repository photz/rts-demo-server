(** Returns the gamestate in which the balance of player whose id
    occurs in the list has been increased by `amount` *)
let gold_to_players gs player_ids amount : Gamestate.t =
  let bump_funds gs player_id =
    let open Gamestate in
    let f = function
      | Some player -> Some (Player.change_balance player amount)
      | _ -> None
    in
    Core.Hashtbl.change gs.players player_id ~f;
    gs
  in
  Core.List.fold player_ids ~init:gs ~f:bump_funds

let transfer_gold time_passed tree gs id : Gamestate.t =
  try 
    let open Component.Resource in
    let f = function
      | Some { amount = current_amount } ->

         let open Component in
         let open Component.Point_mass in
         let pm = Gamestate.point_mass gs id in
         let nearby_bases = Quadtree.within tree pm.position 3. in

         ignore @@ Lwt_io.printf "%d bases in the vicinity of gold mine %d (%.1f|%.1f)\n"
                                 (Core.List.length nearby_bases)
                                 id
                                 pm.position.x
                                 pm.position.y;

         let number_of_bases = Core.List.length nearby_bases in
         
         let amount_per_s_per_player = 0.1 in

         let total_amount = amount_per_s_per_player *. (Core.Float.of_int number_of_bases) in

         let total_amount = Core.min total_amount current_amount in

         let per_player =
           total_amount /. (Core.Float.of_int number_of_bases)
         in

         Lwt_io.printf "giving every player %.1f gold\n" per_player;

         let get_player_id (base_id, _) = Gamestate.ownership gs base_id
         in

         let player_ids = Core.List.map nearby_bases ~f:get_player_id
         in

         let gs = gold_to_players gs player_ids per_player in
         
         Some { amount = current_amount -. total_amount }
      | None -> assert false
    in
    
    Core.Hashtbl.change gs.resources id ~f;

    gs

  with
  | Not_found -> ignore @@ Lwt_io.printf "not found\n"; gs
  | _ -> ignore @@ Lwt_io.printf "error\n"; gs

  

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

  Core.List.fold gold_mine_ids ~init:gs ~f:(transfer_gold time_passed tree)

