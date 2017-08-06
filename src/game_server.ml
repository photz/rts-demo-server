let react (gs : Gamestate.t)
          (client : Websocket_lwt.Connected_client.t)
          (id : int) =
  Lwt_io.printf "new connection from client %d\n" id;
  let barracks_entity_id = Entity.create_barracks gs ~pos:{x=4.0; y=3.0} in
  ignore @@ Lwt_io.printf "created barracks with id %d\n" barracks_entity_id;
  let open Lwt in
  let rec inner () =
    Websocket_lwt.Connected_client.recv client >>= fun frame ->
    match frame.opcode with
    | Websocket_lwt.Frame.Opcode.Ping ->
       Websocket_lwt.Connected_client.send client
                                           Websocket_lwt.Frame.(create ~opcode:Opcode.Pong ~content:frame.content ()) >>=
      inner
    | Websocket_lwt.Frame.Opcode.Close ->
      (* Immediately echo and pass this last message to the user *)
      if String.length frame.content >= 2 then
        let content = String.sub frame.content 0 2 in
        Websocket_lwt.Connected_client.send client Websocket_lwt.Frame.(create ~opcode:Websocket_lwt.Frame.Opcode.Close ~content ())
      else
        Websocket_lwt.Connected_client.send client @@ Websocket_lwt.Frame.close 1000
    | Websocket_lwt.Frame.Opcode.Pong ->
      inner ()
    | Websocket_lwt.Frame.Opcode.Text ->
       ignore @@ Message_handler.handle gs id frame.content;

       let msg = `Assoc [("timestamp", `Int 12345678);
                         ("player_id", `Int 0);
                         ("message", `String "yo whas up??")] in

       let s = `Assoc [("messages", `List [msg])] in

       let json_string = s |> Yojson.to_string in

       let opcode = Websocket_lwt.Frame.Opcode.Text in

       let new_frame = Websocket_lwt.Frame.(create ~opcode ~content:json_string ()) in

       Websocket_lwt.Connected_client.send client new_frame >>= inner
    | _ ->
       let close_frame = Websocket_lwt.Frame.(close 1002) in
       Websocket_lwt.Connected_client.send client close_frame

  in inner ()


let run (gs : Gamestate.t)
        (uri : Uri.t) =
  let open Lwt in
  let id = ref (-1) in
  let handle_conn client =
    incr id;
    let id = !id in
    Lwt.catch
      (fun () -> react gs client id)
      Lwt.fail
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let open Conduit_lwt_unix in
  let endp_str = endp
                 |> Conduit.sexp_of_endp
                 |> Sexplib.Sexp.to_string_hum
  in
  Lwt_io.printf "%s" endp_str >>= fun () ->
  endp_to_server ~ctx:default_ctx endp >>= fun server ->
  let server_str = server
                   |> sexp_of_server
                   |> Sexplib.Sexp.to_string_hum
  in
  Lwt_io.printf "%s" server_str >>= fun () ->
  Websocket_lwt.establish_server ~ctx:default_ctx ~mode:server handle_conn
