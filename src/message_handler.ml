let new_client gs clients client_id send_message =
  let barracks_entity_id = Entity.create_barracks gs ~pos:{x=4.0; y=3.0} in
  ignore @@ Lwt_io.printf "created barracks with id %d\n" barracks_entity_id;
  ignore @@ Core.Hashtbl.add_exn clients
                                 ~key:client_id
                                 ~data:send_message

let create_unit gs clients client_id msg =
  let open Yojson.Basic.Util in
  let entity_id = msg |> member "entity_id" |> to_int in
  let unit_factory = Gamestate.unit_factory gs entity_id in
  Component.Unit_factory.produce unit_factory;
  Lwt_io.printf "create_unit"

let unit_attack gs clients client_id msg =
  Lwt_io.printf "unit_attack"

let unit_go_to gs clients client_id msg =
  Lwt_io.printf "unit_go_to"

let unknown_message gs clients client_id msg =
  Lwt_io.printf "unknown message"

let chat_message gs clients client_id msg =
  let open Yojson.Basic.Util in
  let message = msg |> member "message" |> to_string in
  Lwt_io.printf "chat message: %s\n" message;
  let msg = `Assoc [("timestamp", `Int 12345678);
                    ("player_id", `Int 0);
                    ("message", `String "yo whas up??")] in

  let s = `Assoc [("messages", `List [msg])] in

  let json_string = s |> Yojson.to_string in

  let send_message = Core.Hashtbl.find_exn clients client_id in

  send_message json_string


let get_handler msg =
  let open Yojson.Basic.Util in
  let msg_type = msg |> member "msg_type" in
  match msg_type with
  | `String "create_unit" -> create_unit
  | `String "unit_go_to" -> unit_go_to
  | `String "unit_attack" -> unit_attack
  | `String "chat_message" -> chat_message
  | _ -> unknown_message

let handle gs clients client_id raw_msg =
  try
    let open Lwt in
    let open Yojson.Basic.Util in
    let content = Yojson.Basic.from_string raw_msg in
    let handler = get_handler content in
    handler gs clients client_id content;
  with
    xcp -> Lwt_io.printf "got an invalid message from %d\n" client_id
  
