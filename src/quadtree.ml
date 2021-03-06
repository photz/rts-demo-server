open Vec

(* ^+y 
   |
   |
   +------> +x *)

(** A Quadtree *)
type t =
  | Leaf of { center : vec2;
              size : float;
              entities : (int * vec2) list }
  | Node of { center : vec2;
              size : float;
              left_up : t;
              left_down : t;
              right_up : t;
              right_down : t }

(** The maximum number of entities a leaf can hold *)
let node_capacity = 3

(** Maximum depth of the tree *)
let min_size = 1.0

(** Prints a Quadtree's contents *)
let rec print ?lvl:(lvl=0) = 
  let prefix = String.make (2 * lvl) ' ' in
  function
  | Leaf { entities } ->
     let open Core.Printf in
     Core.List.fold entities ~init:"" ~f:(fun acc (id, pos) ->
                      acc ^ (sprintf "%s%d - %.1f,%.1f\n" prefix id pos.x pos.y))
  | Node { left_up; left_down; right_up; right_down } ->
     let lvl = lvl + 1 in
     let r = prefix ^ "left up\n" in
     let r = r ^ (print ~lvl left_up) in
     let r = r ^ prefix ^ "left down\n" in
     let r = r ^ (print ~lvl left_down) in
     let r = r ^ prefix ^ "right up:\n" in
     let r = r ^ (print ~lvl right_up) in
     let r = r ^ prefix ^ "right down:\n"; in
     r ^ (print ~lvl right_down)



(** Creates a new Quadtree *)
let rec create ?entities:(entities=[]) center size : t =
  let exceeds_capacity = node_capacity < Core.List.length entities in
  let below_min_size = size < min_size in
  match exceeds_capacity, below_min_size with
  | (false, _) | (_, true) ->
     Leaf { center; size; entities }
  | _ -> 
     let child_size = size /. 2. in
     let up = center.y +. child_size in
     let down = center.y -. child_size in
     let left = center.x -. child_size in
     let right = center.x +. child_size in

     let right_up = Leaf { center = { x = right; y = up };
                           size = child_size;
                           entities = [] }
     in

     let right_down = Leaf { center = { x = right; y = down };
                             size = child_size;
                             entities = [] }
     in

     let left_up = Leaf { center = { x = left; y = up };
                          size = child_size;
                          entities = [] }
     in

     let left_down = Leaf { center = { x = left; y = down };
                            size = child_size;
                            entities = [] }
     in

     let tree = Node { center; size; left_up; left_down; right_up; right_down } in
     
     let f tree (id, pos) = match tree with
       | Node { center; size; left_up; right_up; left_down; right_down } -> begin
           let on_rhs_of_center = center.x < pos.x in
           let above_center = center.y < pos.y in
           match above_center, on_rhs_of_center with
           | (true, true) ->
              let right_up = add right_up id pos in
              Node { center; size; left_up; right_up; left_down; right_down }
           | (true, false) ->
              let left_up = add left_up id pos in
              Node { center; size; left_up; right_up; left_down; right_down }
           | (false, true) -> 
              let right_down = add right_down id pos in
              Node { center; size; left_up; right_up; left_down; right_down }
           | (false, false) -> 
              let left_down = add left_down id pos in
              Node { center; size; left_up; right_up; left_down; right_down }
         end
       | _ -> assert false
     in

     Core.List.fold entities ~init:tree ~f 


(** Inserts an entity into the Quadtree *)
and add tree id pos =
  match tree with
  | Leaf { center; size; entities } -> begin
      let entities = (id, pos) :: entities in
      let is_full = node_capacity < Core.List.length entities in
      match is_full with 
      | false -> Leaf { center; size; entities }
      | true -> create ~entities center size
     end
  | Node { center; size; left_up; left_down; right_up; right_down } ->
     let on_rhs_of_center = center.x < pos.x in
     let above_center = center.y < pos.y in
     match above_center, on_rhs_of_center with
     | (true, true) ->
        let right_up = add right_up id pos in
        Node { center; size; left_up; left_down; right_up; right_down }
     | (true, false) ->
        let left_up = add left_up id pos in
        Node { center; size; left_up; left_down; right_up; right_down }
     | (false, true) ->
        let right_down = add right_down id pos in
        Node { center; size; left_up; left_down; right_up; right_down }
     | (false, false) ->
        let left_down = add left_down id pos in
        Node { center; size; left_up; left_down; right_up; right_down }


let circle_square_intersect sqr_center sqr_size circle_center circle_rad =
  let sqr_left_x = sqr_center.x -. sqr_size /. 2. in
  let sqr_right_x = sqr_center.x +. sqr_size /. 2. in
  let closest_x = Core.Float.clamp_exn ~min:sqr_left_x
                                       ~max:sqr_right_x
                                       circle_center.x
  in
  let sqr_top_y = sqr_center.y +. sqr_size /. 2. in
  let sqr_bottom_y = sqr_center.y -. sqr_size /. 2. in
  let closest_y = Core.Float.clamp_exn ~min:sqr_bottom_y
                                       ~max:sqr_top_y
                                       circle_center.y
  in
  let distance_x = circle_center.x -. closest_x in
  let distance_y = circle_center.y -. closest_y in
  let distance_squared = (distance_x *. distance_x) +. (distance_y *. distance_y) in
  distance_squared < (circle_rad *. circle_rad)

(** returns the ids of all entities e such that the distance
    between e's current location and `position` is at most
    `max_distance` *)
let rec within tree position max_distance =
  match tree with
  | Leaf { center; size; entities } ->
     let intersect = circle_square_intersect center (size *. 2.)
                                             position max_distance
     in

     if intersect then (
       Core.List.filter entities ~f:(fun (id, pos) ->
                          distance pos position < max_distance)
     ) else []

  | Node { size; center; left_up; left_down; right_up; right_down } ->
     let intersect = circle_square_intersect center (size *. 2.)
                                             position max_distance
     in
     if intersect then (
       let results = within left_up position max_distance in
       let results = results @ (within left_down position max_distance) in
       let results = results @ (within right_up position max_distance) in
       let results = results @ (within right_down position max_distance) in
       results
     ) else []

(** Remove an entity from the tree by id *)
let rec remove ~id = function
  | Leaf { center; size; entities } ->
     let entities = Core.List.filter entities ~f:(fun (other_id, pos) ->
                                       id != other_id)
     in
     Leaf { center; size; entities }
  | Node { center; size; left_up; right_up; left_down; right_down } ->
     let left_up = remove ~id left_up in
     let left_down = remove ~id left_down in
     let right_up = remove ~id right_up in
     let right_down = remove ~id right_down in
     Node { center; size; left_up; right_up; left_down; right_down }
    
  
