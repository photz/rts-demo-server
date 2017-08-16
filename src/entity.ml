let create_barracks (gs : Gamestate.t) ~pos ~player =
  let unit_factory = Component.Unit_factory.create () in
  let point_mass = Component.Point_mass.create pos in
  let health = Component.Health.create ~max_hp:100.0 in
  Gamestate.create_entity ~unit_factory
                          ~point_mass
                          ~health
                          ~ownership:player
                          gs


let create_unit (gs : Gamestate.t) ~pos ~player =
  let point_mass = Component.Point_mass.create pos in
  let command = Component.Command.create () in
  let health = Component.Health.create ~max_hp:10.0 in
  let armed = Component.Armed.create ~damage:1.0
                                     ~min_dist:0.7
                                     ~reload:1_000_000_000
  in
  Gamestate.create_entity ~command
                          ~point_mass
                          ~health
                          ~armed
                          ~ownership:player
                          gs

