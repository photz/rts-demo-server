let port = 9004
let host = "127.0.0.1"
let tick_ns = 200_000_000
let db_path = "entity_templates.sqlite3"

let () =
  (* message box that the server puts messages into to let the main
     loop know about incoming messages and new clients *)
  let message_box = Lwt_mvar.create_empty () in

  let game_loop =
    let db = Sqlite3.db_open db_path in
    Entity.Template.create_tables db;
    let templates = Entity.Template.all db in
    let gs = Gamestate.create () in

    Game_loop.run tick_ns templates gs message_box
  in

  let server =
    let uri = Uri.make ~scheme:"http" ~host ~port () in

    Game_server.run uri message_box
  in

  Lwt_main.run @@ Lwt.pick [game_loop; server]

  
