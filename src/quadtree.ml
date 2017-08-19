(* ^+y 
   |
   |
   +------> +x *)

type pos = {x : float; y : float }

(** A Quadtree *)
type t =
  | Leaf of { center : pos;
              size : float;
              entities : (int * pos) list }
  | Node of { center : pos;
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
     Core.List.iter entities ~f:(fun (id, pos) ->
                      printf "%s%d - %.1f,%.1f\n" prefix id pos.x pos.y)
  | Node { left_up; left_down; right_up; right_down } ->
     let lvl = lvl + 1 in
     print_endline @@ prefix ^ "left up:";
     print ~lvl left_up;
     print_endline @@ prefix ^ "left down:";
     print ~lvl left_down;
     print_endline @@ prefix ^ "right up:";
     print ~lvl right_up;
     print_endline @@ prefix ^ "right down:";
     print ~lvl right_down

(** Determines the distance between two points in 2d *)                   
let distance a b =
  let x_d = a.x -. b.x in
  let y_d = a.y -. b.y in
  Core.Float.sqrt(x_d *. x_d +. y_d *. y_d)


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
     let intersect = circle_square_intersect center size
                                             position max_distance
     in

     if intersect then (
       Core.List.filter entities ~f:(fun (id, pos) ->
                          distance pos position < max_distance)
     ) else []

  | Node { size; center; left_up; left_down; right_up; right_down } ->
     let intersect = circle_square_intersect center size
                                             position max_distance
     in
     if intersect then (
       let results = within left_up position max_distance in
       let results = results @ (within right_up position max_distance) in
       let results = results @ (within left_down position max_distance) in
       let results = results @ (within left_up position max_distance) in
       results
     ) else []
     
