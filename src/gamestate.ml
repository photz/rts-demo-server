type t = {
    point_masses: (int, Component.Point_mass.t) Core.Hashtbl.t;
    commands: (int, Component.Command.t) Core.Hashtbl.t;
    unit_factories: (int, Component.Unit_factory.t) Core.Hashtbl.t;
    mutable entity_id: int;
  }

let create () = { point_masses=Core.Int.Table.create ();
                  commands=Core.Int.Table.create ();
                  unit_factories=Core.Int.Table.create ();
                  entity_id=0;
                }

let new_entity_id (gs : t) =
  gs.entity_id <- gs.entity_id + 1;
  gs.entity_id

let unit_factory gs (entity_id : int) =
  Core.Hashtbl.find_exn gs.unit_factories entity_id

let unit_factories gs = gs.unit_factories

(** creates a new entity with the given components and returns its id *)
let create_entity gs ?unit_factory ?point_mass =
  let entity_id = new_entity_id gs in

  begin
    match unit_factory with 
    | Some x ->
       ignore @@ Core.Hashtbl.add gs.unit_factories
                                  ~key:entity_id ~data:x
    | None -> ()
  end;

  begin
    match point_mass with
    | Some x ->
       ignore @@ Core.Hashtbl.add gs.point_masses
                                  ~key:entity_id ~data:x
    | None -> ()
  end;

  (* begin *)
  (*   match command with *)
  (*   | Some x -> *)
  (*      ignore @@ Core.Hashtbl.add gs.commands *)
  (*                                 ~key:entity_id ~data:x *)
  (*   | None -> () *)
  (* end; *)

  entity_id
