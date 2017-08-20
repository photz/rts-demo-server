type t = {
    name: string;
    funds: float;
  }

let create name funds = { name = name; funds = funds }

let serialize (player : t) =
  let open Yojson.Basic in
  `Assoc [("name", `String player.name);
         ("funds", `Float player.funds)]
