let ns_to_s ns =
  let ns_per_s = 1_000_000_000. in
  (Core.Float.of_int ns) /. ns_per_s

let rec run tick_ns gs message_box last_update_ns =
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
     System.run gs (ns_to_s tick_ns);
     run tick_ns gs message_box (Util.get_timestamp ())
  | Some message ->
     Message_handler.handle gs message;
     run tick_ns gs message_box last_update_ns


let () =
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9003 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  let clients = Core.Int.Table.create () in

  let message_box : string Lwt_mvar.t = Lwt_mvar.create_empty () in

  let tick_ns = 1_000_000_000 in

  Lwt_main.run @@ Lwt.join [run tick_ns gs message_box 0;
                            Game_server.run uri message_box clients]
  
