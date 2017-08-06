type vec2 = { x: float; y: float }

module Point_mass = struct
  type t = { position: vec2;
             velocity: vec2;
           }

  let create pos =
    { position=pos; velocity={x=0.; y=0.}}
end

module Command = struct
  type t =
    | Idle
    | GoTo of vec2
    | Attack of int
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
end
