let new_client gs client_id send =
  let open Yojson.Basic in
  let msg = `Assoc [("player_id", `Int client_id)] in
  send @@ Yojson.to_string msg;
  let x = Core.Random.float 30.0 in
  let y = Core.Random.float 30.0 in
  let open Component in
  let barracks_entity_id = Entity.create_barracks gs ~pos:{x; y} ~player:client_id in
  let open Gamestate in
  let new_player = Player.create "unknown" 3000. in
  Core.Hashtbl.add_exn gs.players client_id new_player;
  Entity.create_gold_mine gs ~pos:{x=x+.5.0;y};
  gs

(** Handler that gets called when a player leaves the game *)
let leave gs client_id : Gamestate.t =
  (* get a list of entities belonging to the player who just left *)
  let entity_ids = ref [] in

  let f ~key ~data =
    if data == client_id
    then entity_ids := key :: !entity_ids
  in

  let open Gamestate in
  Core.Hashtbl.iteri gs.ownership ~f;

  Core.List.fold !entity_ids ~init:gs ~f:Gamestate.remove_entity


let create_unit entity_templates gs clients client_id msg : Gamestate.t =
  let open Yojson.Basic.Util in
  let open Player in
  let open Component.Unit_factory in
  let entity_id = msg |> member "entity_id" |> to_int in
  let player = Gamestate.player gs client_id in
  let template_id = msg |> member "template_id" |> to_int in
  let template = Core.Map.find entity_templates template_id in
  let factory = Gamestate.unit_factory gs entity_id in
  let unit_price = Core.List.Assoc.find_exn ~equal:(=)
                                            factory.producibles
                                            template_id
  in

  let sufficient_funds = Core.Float.of_int unit_price <= player.funds in
  begin
    match sufficient_funds with 
    | true ->
       let open Gamestate in
       let p = Gamestate.player gs client_id in
       Core.Hashtbl.change gs.players client_id ~f:(fun _ ->
                             Some {name=p.name;
                                   funds=p.funds-.(Core.Float.of_int unit_price)});

       let factory = Component.Unit_factory.produce
                       factory
                       template_id
       in

       Core.Hashtbl.change gs.unit_factories entity_id ~f:(fun _ ->
                             Some factory)

    | false ->
       ignore @@ Lwt_io.printf "player %d has insufficient funds to produce a unit\n"
  end;
  gs

let unit_attack entity_templates gs clients client_id msg =
  let open Gamestate in
  let open Yojson.Basic.Util in
  let entity_id = msg |> member "entity_id" |> to_int in
  let target = msg |> member "target" |> to_int in

  let owner_id = Core.Hashtbl.find_exn gs.ownership entity_id in

  let open Component.Command in
  let f = function
    | None -> None
    | Some _ -> Some (Attack target)
  in

  Core.Hashtbl.change gs.commands entity_id ~f;

  Lwt_io.printf "player %d attacking %d with %d\n"
                client_id
                target
                entity_id;

  gs


let unit_go_to entity_templates gs clients client_id msg : Gamestate.t =
  let open Yojson.Basic.Util in
  let entity_id = msg |> member "entity_id" |> to_int in
  let dest_x = msg |> member "dest" |> member "x" |> to_float in
  let dest_y = msg |> member "dest" |> member "y" |> to_float in

  let open Gamestate in
  let open Component in
  let open Component.Command in

  let owner_id = Core.Hashtbl.find_exn gs.ownership entity_id in

  let f = function
    | None -> None
    | Some current_command ->
       if owner_id == client_id
       then Some (GoTo {x=dest_x; y=dest_y})
       else Some current_command
  in

  Core.Hashtbl.change gs.commands entity_id ~f;

  gs

let unknown_message _ gs clients client_id msg = gs
                                             

let chat_message _ gs clients client_id msg =
  let open Yojson.Basic.Util in
  let message = msg |> member "message" |> to_string in
  let msg = `Assoc [("player_id", `Int client_id);
                    ("message", `String message)] in

  let msg = `Assoc [("messages", `List [msg])] in

  let json_string = msg |> Yojson.to_string in

  Core.Hashtbl.iter_vals clients ~f:(fun send ->
                           send json_string);

  gs

let get_handler msg =
  let open Yojson.Basic.Util in
  let msg_type = msg |> member "msg_type" in
  match msg_type with
  | `String "create_unit" -> create_unit
  | `String "unit_go_to" -> unit_go_to
  | `String "unit_attack" -> unit_attack
  | `String "chat_msg" -> chat_message
  | _ -> unknown_message

(** Handles messages put into its message box by other threads *)
let handle entity_templates gs msg clients client_id =
  try
    let content = Yojson.Basic.from_string msg in
    let handler = get_handler content in
    handler entity_templates gs clients client_id content
  with
  | Yojson.Json_error x ->
     Lwt_io.printf "error while parsing json from client: %s\n" x;
     gs
  | Failure msg ->
     Lwt_io.printf "error: %s" msg;
     gs
  | _ ->
     Lwt_io.printf "exception while handling message from client\n";
     gs
