let create_barracks (gs : Gamestate.t) ~pos =
  let unit_factory = Component.Unit_factory.create () in
  let point_mass = Component.Point_mass.create pos in
  Gamestate.create_entity gs ~unit_factory ~point_mass
