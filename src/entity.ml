let create_barracks (gs : Gamestate.t) ~pos ~player =
  let unit_factory = Component.Unit_factory.create () in
  let point_mass = Component.Point_mass.create pos in
  Gamestate.create_entity gs ~unit_factory
                          ~point_mass
                          ?command:None
                          ?ownership:player

let create_unit (gs : Gamestate.t) ~pos ~player =
  let point_mass = Component.Point_mass.create pos in
  let command = Component.Command.create () in
  Gamestate.create_entity gs ~command
                          ~point_mass
                          ?unit_factory:None
                          ?ownership:player
