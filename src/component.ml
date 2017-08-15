type vec2 = { x: float; y: float }

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
  type t = { hp: float }
end

(** A component encapsulating an entity's ability to 
    inflict damage on other entities *)
module CanAttack = struct
  type t = { damage: float }
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
