let ns_to_s ns =
  let ns_per_s = 1_000_000_000. in
  (Core.Float.of_int ns) /. ns_per_s

let rec run tick_ns gs message_box last_update_ns clients =
  let open Lwt in
  
  let now = Util.get_timestamp () in
  let time_passed_ns = now - last_update_ns in
  let time_left_ns = tick_ns - time_passed_ns in

  let tick = Lwt_unix.sleep (ns_to_s time_left_ns) >>= fun () ->
             Lwt.return None
  in

  let handle_msg = Lwt_mvar.take message_box >>= fun message ->
                   Lwt.return (Some message)
  in

  Lwt.pick [tick; handle_msg] >>= function
  | None ->
     let gs = System.run gs (ns_to_s tick_ns) in
     Lwt_io.printf "currently %d clients\n"
                   (Core.Hashtbl.length clients);
     run tick_ns gs message_box (Util.get_timestamp ()) clients
  | Some message ->
     begin
       match message with
       | Game_server.Message.Join (client_id, send) ->
          Core.Hashtbl.add_exn clients client_id send;
          let gs = Message_handler.new_client gs client_id in
          Lwt_io.printf "new client %d\n" client_id;
          run tick_ns gs message_box last_update_ns clients
         
       | Game_server.Message.Quit client_id -> 
          Core.Hashtbl.remove clients client_id;
          Lwt_io.printf "client %d is leaving us :-(\n" client_id;
          run tick_ns gs message_box last_update_ns clients

       | Game_server.Message.Message (id, text) ->
          Lwt_io.printf "text: %s\n" text;
          let gs = Message_handler.handle gs text in
          run tick_ns gs message_box last_update_ns clients

     end

let () =
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9003 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  let clients = Core.Int.Table.create () in

  let message_box : Game_server.Message.t Lwt_mvar.t = Lwt_mvar.create_empty () in

  let tick_ns = 1_000_000_000 in

  Lwt_main.run @@ Lwt.join [run tick_ns gs message_box 0 clients;
                            Game_server.run uri message_box]
  
