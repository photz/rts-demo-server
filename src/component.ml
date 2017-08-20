open Vec

module Point_mass = struct
  type t = { position: vec2;
             velocity: vec2;
           }

  let create pos =
    { position=pos; velocity={x=0.; y=0.} }

  let update_pos pm time_passed =
    let x = pm.position.x +. pm.velocity.x *. time_passed in
    let y = pm.position.y +. pm.velocity.y *. time_passed in
    { position={x;y}; velocity=pm.velocity}

  let update_velocity pm velocity =
    { position = pm.position; velocity }

  let halt pm =
    { position = pm.position; velocity = {x = 0.; y = 0. } }

  let serialize pm =
    let open Yojson.Basic in
    let position = `Assoc [("x", `Float pm.position.x);
                           ("y", `Float pm.position.y)]
    in
    let velocity = `Assoc [("x", `Float pm.velocity.x);
                           ("y", `Float pm.velocity.y)]
    in

    `Assoc [("position", position);
            ("velocity", velocity)]

  let distance (pm1 : t) (pm2 : t) : float =
    let x_d = pm1.position.x -. pm2.position.x in
    let y_d = pm1.position.y -. pm2.position.y in
    Core.Float.sqrt @@ (x_d *. x_d) +. (y_d *. y_d)
end


module Command = struct
  type t =
    | Idle
    | GoTo of vec2
    | Attack of int

  let create () = Idle

  let serialize (command : t) =
    let open Yojson.Basic in
    match command with
    | Idle -> `String "idle"
    | GoTo _ -> `String "walking"
    | Attack _ -> `String "attacking"
end

(** Component encapsulating an entity's health or repair state *)
module Health = struct
  type t = { hp: float;
             max_hp: float }

  let create ~max_hp =
    { hp = max_hp; max_hp }

  let serialize (health : t) =
    let open Yojson.Basic in
    `Assoc [("hp", `Float health.hp);
            ("max_hp", `Float health.max_hp)]
end

(** A component encapsulating an entity's ability to 
    inflict damage on other entities *)
module Armed = struct
  type t = { damage: float;
             min_dist: float;
             reload: int;
             last_shot: int }

  let create ~damage ~min_dist ~reload =
    { damage; min_dist; reload; last_shot = 0 }

  let serialize (c : t) =
    let open Yojson.Basic in
    `Assoc [("damage", `Float c.damage);
            ("min_dist", `Float c.min_dist)]
end

module Unit_factory = struct
  type t = { queue: int Core.Queue.t }

  let create () = { queue = Core.Queue.create () }

  let produce (unit_factory : t) =
    let now : int = Util.get_timestamp () in
    Core.Queue.enqueue unit_factory.queue now

  let units_in_queue (unit_factory : t) =
    Core.Queue.length unit_factory.queue

  let peek (unit_factory : t) =
    Core.Queue.peek unit_factory.queue

  let remove_first (unit_factory : t) =
    Core.Queue.dequeue unit_factory.queue

  let serialize (unit_factory : t) =
    `Assoc [("in_queue", `Int (Core.Queue.length unit_factory.queue))]
end

module Resource = struct
  type t = { amount: int }

  let create amount = { amount }

  let serialize (resource : t) =
    `Assoc [("amount", `Int resource.amount)]
end
