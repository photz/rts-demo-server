let create_unit gs client_id msg =
  let open Yojson.Basic.Util in
  let entity_id = msg |> member "entity_id" |> to_int in
  let unit_factory = Gamestate.unit_factory gs entity_id in
  Component.Unit_factory.produce unit_factory;
  Lwt_io.printf "create_unit"

let unit_attack gs client_id msg =
  Lwt_io.printf "unit_attack"

let unit_go_to gs client_id msg =
  Lwt_io.printf "unit_go_to"

let unknown_message gs client_id msg =
  Lwt_io.printf "unknown message"

let chat_message gs client_id msg =
  let open Yojson.Basic.Util in
  let message = msg |> member "message" |> to_string in
  Lwt_io.printf "chat message: %s\n" message

let get_handler msg =
  let open Yojson.Basic.Util in
  let msg_type = msg |> member "msg_type" in
  match msg_type with
  | `String "create_unit" -> create_unit
  | `String "unit_go_to" -> unit_go_to
  | `String "unit_attack" -> unit_attack
  | `String "chat_message" -> chat_message
  | _ -> unknown_message

let handle gs client_id raw_msg =
  try
    let open Lwt in
    let open Yojson.Basic.Util in
    let content = Yojson.Basic.from_string raw_msg in
    let handler = get_handler content in
    handler gs client_id content
  with
    xcp -> Lwt_io.printf "got an invalid message from %d\n" client_id
  
