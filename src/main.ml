
let () =
  let entity_templates =
    let db = Sqlite3.db_open "entity_templates.sqlite3" in
    Entity.Template.all db
  in
  let gs = Gamestate.create () in
  let port = Core.Int.to_string 9004 in
  let uri = Uri.of_string @@ "http://127.0.0.1:" ^ port in

  let message_box : Game_server.Message.t Lwt_mvar.t = Lwt_mvar.create_empty () in

  let tick_ns = 200_000_000 in

  let loop = Game_loop.run tick_ns entity_templates gs message_box
  in

  let server = Game_server.run uri message_box in

  Lwt_main.run @@ Lwt.pick [loop; server]

  
