let new_client gs client_id =
  let barracks_entity_id = Entity.create_barracks gs ~pos:{x=4.0; y=3.0} in
  ignore @@ Lwt_io.printf "created barracks with id %d\n" barracks_entity_id;
  (* ignore @@ Core.Hashtbl.add_exn clients *)
  (*                                ~key:client_id *)
  (*                                ~data:send_message; *)

  gs

let create_unit gs clients client_id msg : Gamestate.t =
  let open Yojson.Basic.Util in
  let entity_id = msg |> member "entity_id" |> to_int in
  let unit_factory = Gamestate.unit_factory gs entity_id in
  ignore @@ Component.Unit_factory.produce unit_factory;
  gs

let unit_attack gs clients client_id msg = gs


let unit_go_to gs clients client_id msg = gs


let unknown_message gs clients client_id msg = gs


let chat_message gs clients client_id msg =
  let open Yojson.Basic.Util in
  let message = msg |> member "message" |> to_string in
  let msg = `Assoc [("timestamp", `Int 12345678);
                    ("player_id", `Int 0);
                    ("message", `String "yo whas up??")] in

  let s = `Assoc [("messages", `List [msg])] in

  let json_string = s |> Yojson.to_string in

  let send_message = Core.Hashtbl.find_exn clients client_id in

  ignore @@ send_message json_string;

  gs


let get_handler msg =
  let open Yojson.Basic.Util in
  let msg_type = msg |> member "msg_type" in
  match msg_type with
  | `String "create_unit" -> create_unit
  | `String "unit_go_to" -> unit_go_to
  | `String "unit_attack" -> unit_attack
  | _ -> unknown_message

(** Handles messages put into its message box by other threads *)
let handle gs msg =
  try
    let content = Yojson.Basic.from_string msg in
    let handler = get_handler content in
    handler gs (Core.Int.Table.create ()) 123 content
  with
    xcn -> gs

