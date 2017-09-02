(** Turns a Hashtbl into a serializable Yojson Assoc *)
let yojson_hashtbl tbl g =
  let open Yojson.Basic in

  let init = [] in

  let f ~key ~data acc = (Core.Int.to_string key, g data) :: acc  in

  `Assoc (Core.Hashtbl.fold tbl ~f ~init)

(** Transmits the given game state to every player *)
let update_players gs clients =

  let open Gamestate in

  let point_masses = yojson_hashtbl (Gamestate.point_masses gs)
                                    Component.Point_mass.serialize
  in

  let commands = yojson_hashtbl (Gamestate.commands gs)
                                Component.Command.serialize
  in

  let unit_factories = yojson_hashtbl (Gamestate.unit_factories gs)
                                      Component.Unit_factory.serialize
  in

  let ownership = yojson_hashtbl gs.ownership (fun owner_id ->
                                   `Int (owner_id))
  in

  let health = yojson_hashtbl gs.health Component.Health.serialize
  in

  let armed = yojson_hashtbl gs.armed Component.Armed.serialize
  in

  let resources = yojson_hashtbl gs.resources
                                 Component.Resource.serialize
  in

  let players = yojson_hashtbl gs.players Player.serialize in

  let gs_yojson = `Assoc [("point_masses", point_masses);
                          ("commands", commands);
                          ("unit_factories", unit_factories);
                          ("ownership", ownership);
                          ("health", health);
                          ("armed", armed);
                          ("players", players);
                          ("resources", resources)]
  in

  let f ~key ~data = data (Yojson.to_string gs_yojson) in

  Core.Hashtbl.iteri clients ~f


let rec run_internal tick_ns entity_templates  gs message_box last_update_ns clients =

  let open Lwt in
  
  let now = Util.get_timestamp () in
  let time_passed_ns = now - last_update_ns in
  let time_left_ns = tick_ns - time_passed_ns in

  let tick = Lwt_unix.sleep (Util.ns_to_s time_left_ns) >>= fun () ->
             Lwt.return None
  in

  let handle_msg = Lwt_mvar.take message_box >>= fun message ->
                   Lwt.return (Some message)
  in

  Lwt.pick [tick; handle_msg] >>= function
  | None ->
     let gs = System.run entity_templates gs (Util.ns_to_s tick_ns) in
     Lwt_io.printf "currently %d clients\n"
                   (Core.Hashtbl.length clients);
     update_players gs clients;
     run_internal tick_ns entity_templates gs message_box (Util.get_timestamp ()) clients
  | Some message ->
     begin
       match message with
       | Game_server.Message.Join (client_id, send) ->
          Core.Hashtbl.add_exn clients client_id send;
          let gs = Message_handler.new_client gs client_id send in
          Lwt_io.printf "new client %d\n" client_id;
          run_internal tick_ns entity_templates gs message_box last_update_ns clients
         
       | Game_server.Message.Quit client_id -> 
          Core.Hashtbl.remove clients client_id;
          let gs = Message_handler.leave gs client_id in
          run_internal tick_ns entity_templates gs message_box last_update_ns clients

       | Game_server.Message.Message (id, text) ->
          Lwt_io.printf "text: %s\n" text;
          let gs = Message_handler.handle entity_templates gs text clients id in
          run_internal tick_ns entity_templates gs message_box last_update_ns clients
     end

let run tick_ns entity_templates gs message_box =
  let clients = Core.Int.Table.create () in

  run_internal tick_ns entity_templates gs message_box 0 clients
