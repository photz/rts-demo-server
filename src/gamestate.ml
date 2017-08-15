type t = {
    point_masses: (int, Component.Point_mass.t) Core.Hashtbl.t;
    commands: (int, Component.Command.t) Core.Hashtbl.t;
    unit_factories: (int, Component.Unit_factory.t) Core.Hashtbl.t;
    ownership: (int, int) Core.Hashtbl.t;
    mutable entity_id: int;
  }

let create () = { point_masses=Core.Int.Table.create ();
                  commands=Core.Int.Table.create ();
                  unit_factories=Core.Int.Table.create ();
                  ownership=Core.Int.Table.create ();
                  entity_id=0;
                }

let unit_factory gs (entity_id : int) =
  Core.Hashtbl.find_exn gs.unit_factories entity_id

let unit_factories gs = gs.unit_factories

let point_masses gs = gs.point_masses

let commands gs = gs.commands

let point_mass gs entity_id =
  Core.Hashtbl.find_exn gs.point_masses entity_id

(** creates a new entity with the given components and returns its id *)
let create_entity gs ?unit_factory ?point_mass ?command ~ownership =
  gs.entity_id <- gs.entity_id + 1;

  let entity_id = gs.entity_id in

  begin
    match unit_factory with 
    | Some x ->
       Core.Hashtbl.add_exn gs.unit_factories
                                  ~key:entity_id ~data:x
    | None -> ()
  end;

  begin
    match point_mass with
    | Some x ->
       Core.Hashtbl.add_exn gs.point_masses
                                  ~key:entity_id ~data:x
    | None -> ()
  end;

  begin
    match command with
    | Some x ->
       Core.Hashtbl.add_exn gs.commands
                            ~key:entity_id ~data:x
    | None -> ()
  end;

  Core.Hashtbl.add_exn gs.ownership
                       ~key:entity_id
                       ~data:ownership;

  entity_id
