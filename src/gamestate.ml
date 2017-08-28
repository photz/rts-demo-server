type t = {
    point_masses: (int, Component.Point_mass.t) Core.Hashtbl.t;
    commands: (int, Component.Command.t) Core.Hashtbl.t;
    unit_factories: (int, Component.Unit_factory.t) Core.Hashtbl.t;
    ownership: (int, int) Core.Hashtbl.t;
    health: (int, Component.Health.t) Core.Hashtbl.t;
    armed: (int, Component.Armed.t) Core.Hashtbl.t;
    resources: (int, Component.Resource.t) Core.Hashtbl.t;
    mutable entity_id: int;
    players: (int, Player.t) Core.Hashtbl.t;
  }

let create () = { point_masses=Core.Int.Table.create ();
                  commands=Core.Int.Table.create ();
                  unit_factories=Core.Int.Table.create ();
                  ownership=Core.Int.Table.create ();
                  health=Core.Int.Table.create ();
                  armed=Core.Int.Table.create ();
                  resources=Core.Int.Table.create ();
                  entity_id=0;
                  players=Core.Int.Table.create ();
                }

let player gs id = Core.Hashtbl.find_exn gs.players id

let unit_factory gs (entity_id : int) =
  Core.Hashtbl.find_exn gs.unit_factories entity_id

let unit_factories gs = gs.unit_factories

let point_masses gs = gs.point_masses

let commands gs = gs.commands

let point_mass gs entity_id =
  Core.Hashtbl.find_exn gs.point_masses entity_id

let armed gs entity_id =
  Core.Hashtbl.find_exn gs.armed entity_id

let ownership gs entity_id =
  Core.Hashtbl.find_exn gs.ownership entity_id

let remove_entity gs entity_id =
  Core.Hashtbl.remove gs.point_masses entity_id;
  Core.Hashtbl.remove gs.unit_factories entity_id;
  Core.Hashtbl.remove gs.health entity_id;
  Core.Hashtbl.remove gs.armed entity_id;
  Core.Hashtbl.remove gs.commands entity_id;
  Core.Hashtbl.remove gs.ownership entity_id;
  Core.Hashtbl.remove gs.resources entity_id

(** creates a new entity with the given components and returns its id *)
let create_entity ?armed ?unit_factory ?point_mass ?command ?ownership ?health ?resource gs =
  gs.entity_id <- gs.entity_id + 1;

  let entity_id = gs.entity_id in

  begin
    match armed with
    | Some armed -> Core.Hashtbl.add_exn gs.armed
                                         ~key:entity_id
                                         ~data:armed
    | None -> ()
  end;

  begin
    match unit_factory with 
    | Some x -> Core.Hashtbl.add_exn gs.unit_factories
                                     ~key:entity_id ~data:x
    | None -> ()
  end;

  begin
    match point_mass with
    | Some x -> Core.Hashtbl.add_exn gs.point_masses
                                     ~key:entity_id ~data:x
    | None -> ()
  end;

  begin
    match command with
    | Some x -> Core.Hashtbl.add_exn gs.commands
                                     ~key:entity_id ~data:x

    | None -> ()
  end;

  begin
    match ownership with
    | Some ownership -> Core.Hashtbl.add_exn gs.ownership
                                             ~key:entity_id
                                             ~data:ownership
    | None -> ()
  end;

  begin
    match health with
    | Some health -> Core.Hashtbl.add_exn gs.health
                                          ~key:entity_id
                                          ~data:health
    | None -> ()
  end;

  begin
    match resource with
    | Some resource -> Core.Hashtbl.add_exn gs.resources
                                            ~key:entity_id
                                            ~data:resource
    | None -> ()
  end;

  entity_id
