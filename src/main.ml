let () =
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9003 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  Lwt_main.run @@ Lwt.join [Game_loop.run gs; Game_server.run gs uri] 


