let () =
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9003 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  let clients = Core.Int.Table.create () in

  Lwt_main.run @@ Lwt.join [Game_loop.run gs clients;
                            Game_server.run
                              uri
                              (Message_handler.new_client gs clients)
                              (Message_handler.handle gs clients)]


