let reply (client : Websocket_lwt.Connected_client.t)
          (content : string) =
  let opcode = Websocket_lwt.Frame.Opcode.Text in

  let frame = Websocket_lwt.Frame.create ~opcode ~content () in

  Websocket_lwt.Connected_client.send client frame
  

let react (client : Websocket_lwt.Connected_client.t)
          (id : int)
          new_client_handler
          new_message_handler =
  Lwt_io.printf "new connection from client %d\n" id;
  new_client_handler id (reply client);
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
       new_message_handler id frame.content;
       inner ()
    | _ ->
       let close_frame = Websocket_lwt.Frame.(close 1002) in
       Websocket_lwt.Connected_client.send client close_frame

  in inner ()


let run (uri : Uri.t)
        new_client_handler
        new_message_handler =
  let open Lwt in
  let id = ref (-1) in
  let handle_conn client =
    incr id;
    let id = !id in
    Lwt.catch
      (fun () -> react client id new_client_handler new_message_handler)
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
