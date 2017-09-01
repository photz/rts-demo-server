
(** Apply all systems and return the resulting game state *)
let run entity_templates gs time_passed : Gamestate.t =
  let systems = [| Unit_production.run; Commands.run; Movement.run; Resource_production.run |] in

  let f gs system = system entity_templates gs time_passed in

  Core.Array.fold systems ~init:gs ~f
  
