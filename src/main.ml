let () =
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9004 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  let message_box : Game_server.Message.t Lwt_mvar.t = Lwt_mvar.create_empty () in

  let tick_ns = 200_000_000 in

  Lwt_main.run @@ Lwt.pick [Game_loop.run tick_ns gs message_box;
                            Game_server.run uri message_box]
  
