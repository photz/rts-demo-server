type t = {
    name: string;
    funds: int;
  }

let create name funds = { name = name; funds = funds }

let serialize (player : t) =
  let open Yojson.Basic in
  `Assoc [("name", `String player.name);
         ("funds", `Int player.funds)]
