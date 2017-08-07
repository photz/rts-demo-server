let () =
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9003 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  let clients = Core.Int.Table.create () in

  let message_box : string Lwt_mvar.t = Lwt_mvar.create_empty () in

  Lwt_main.run @@ Lwt.join [Game_loop.run gs clients;
                            Game_server.run uri message_box clients;
                            Message_handler.loop gs message_box clients]


