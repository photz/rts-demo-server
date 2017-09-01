module Template = struct
  type t = { point_mass: Component.Point_mass.t option;
             health: Component.Health.t option;
             armed: Component.Armed.t option;
             command: Component.Command.t option;
             unit_factory: Component.Unit_factory.t option;
             resource: Component.Resource.t option }

  let create_entity_template_table db =
    try
      let sql = "CREATE TABLE entity_template(
                 id UNSIGNED INTEGER,
                 name VARCHAR,
                 PRIMARY KEY(id)
                 )"
      in
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | _ -> assert false
    with
      xcp -> print_endline "unable to create table entity_templates"

  let create_health_table db =
    try
      let sql = "CREATE TABLE health(
                 entity_template_id UNSIGNED INTEGER,
                 max_hp UNSIGNED INTEGER,
                 FOREIGN KEY(entity_template_id)
                 REFERENCES entity_template(id)
                 )"
      in
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | _ -> assert false
    with
      xcp -> print_endline "unable to create table health"

  let create_armed_table db =
    try
      let sql = "CREATE TABLE armed(
                 entity_template_id UNSIGNED INTEGER,
                 damage UNSIGNED DECIMAL,
                 min_dist UNSIGNED DECIMAL,
                 reload UNSIGNED INTEGER,
                 FOREIGN KEY(entity_template_id)
                 REFERENCES entity_template(id)
                 )"
      in
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | _ -> assert false
    with
      xcp -> print_endline "unable to create table armed"

  let create_producible_table db =
    try
      let sql = "CREATE TABLE producible(
                 entity_template_id UNSIGNED INTEGER,
                 cost UNSIGNED INTEGER,
                 FOREIGN KEY(entity_template_id)
                 REFERENCES entity_template(id)
                 )"
      in
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | _ -> assert false
    with
      xcp -> print_endline "unable to create producible table"

  let create_produces_table db =
    try
      let sql = "CREATE TABLE produces(
                 entity_template_id UNSIGNED INTEGER,
                 producible UNSIGNED INTEGER,
                 FOREIGN KEY(entity_template_id)
                 REFERENCES entity_template(id),
                 FOREIGN KEY(producible)
                 REFERENCES entity_template(id)
                 )"
      in
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | _ -> assert false
    with
      xcp -> print_endline "unable to create table"


  let get_armed db template_id =
    let armed = ref None in
    let cb = function
      | [| Some damage; Some min_dist; Some reload |] ->
         let damage = Core.Float.of_string damage in
         let min_dist = Core.Float.of_string min_dist in
         let reload = Core.Int.of_string reload in
         armed := Some (Component.Armed.create ~damage ~min_dist ~reload)
      | _ -> ()
    in
    let sql =
      "SELECT damage, min_dist, reload
       FROM armed
       WHERE entity_template_id = " ^ (Core.Int.to_string template_id)
    in
    match Sqlite3.exec_no_headers db sql ~cb with
    | Sqlite3.Rc.OK -> !armed
    | _ -> raise (Failure "unable to obtain component from db")

  let get_health db template_id =
    let health = ref None in
    let cb = function
      | [| Some max_hp |] ->
         let max_hp = Core.Float.of_string max_hp in
         health := Some (Component.Health.create ~max_hp)
      | _ -> ()
    in
    let sql =
      "SELECT max_hp
       FROM health
       WHERE entity_template_id = " ^ (Core.Int.to_string template_id)
    in
    match Sqlite3.exec_no_headers db sql ~cb with
    | Sqlite3.Rc.OK -> !health
    | _ -> raise (Failure "unable to get health component")

  let get_factory db id : Component.Unit_factory.t option =
    let producibles = ref [] in

    let cb = function
      | [| Some id; Some cost |] ->
         let id = Core.Int.of_string id in
         let cost = Core.Int.of_string cost in
         producibles := (id, cost) :: !producibles
      | _ -> ()
    in
    
    let sql =
      "SELECT p.producible, p2.cost
       FROM produces p JOIN producible p2
       ON p.producible = p2.entity_template_id
       WHERE p.entity_template_id = " ^ (Core.Int.to_string id)
    in

    match Sqlite3.exec_no_headers db sql ~cb with
    | Sqlite3.Rc.OK ->
       if 0 == (Core.List.length !producibles)
       then None
       else Some (Component.Unit_factory.create !producibles)
    | status ->
       print_endline @@ Sqlite3.Rc.to_string status;
       None

  let create_tables db =
    Sqlite3.exec db "PRAGMA foreign_keys = ON";
    create_entity_template_table db;
    create_health_table db;
    create_armed_table db;
    create_producible_table db;
    create_produces_table db

  let get db id : t =
    { point_mass = None;
      health = get_health db id;
      armed = get_armed db id;
      command = None;
      unit_factory = get_factory db id;
      resource = None }

  let spawn ?point_mass ?command ?ownership gs (template : t) =
    let entity_id = Gamestate.create_entity ?armed:template.armed
                            ?health:template.health
                            ?unit_factory:template.unit_factory
                            ?command
                            ?ownership
                            ?point_mass
                            gs
    in
    gs

  let all db =
    let template_ids = ref [] in

    let cb = function
      | [| Some template_id |] ->
         let template_id = Core.Int.of_string template_id in
         template_ids := template_id :: !template_ids
    in

    let sql = "SELECT id FROM entity_template" in

    Sqlite3.exec_no_headers db sql ~cb;

    let f map template_id =
      let template = get db template_id in
      Core.Map.add map ~key:template_id ~data:template
    in

    let init = Core.Int.Map.empty in

    Core.List.fold ~init !template_ids  ~f
end

let create_barracks (gs : Gamestate.t) ~pos ~player =
  let db = Sqlite3.db_open "entity_templates.sqlite3" in
  let point_mass = Component.Point_mass.create pos in
  let barracks = Template.get db 4 in
  Template.spawn ~point_mass ~ownership:player gs barracks

let create_gold_mine gs ~pos =
  let point_mass = Component.Point_mass.create pos in
  let resource = Component.Resource.create 2000. in
  Gamestate.create_entity ~point_mass
                          ~resource
                          gs
  
