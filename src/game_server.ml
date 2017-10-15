module Message = struct
  type t = Join of (int * (string -> unit))
         | Message of (int * string)
         | Quit of int
end

let reply (client : Websocket_lwt.Connected_client.t)
          (content : string) : unit =
  let opcode = Websocket_lwt.Frame.Opcode.Text in

  let frame = Websocket_lwt.Frame.create ~opcode ~content () in

  Websocket_lwt.Connected_client.send client frame;

  ()
  

let react (client : Websocket_lwt.Connected_client.t)
          (id : int)
          message_box =
  Lwt_io.printf "new connection from client %d\n" id;
  let open Message in
  let join_msg = Join (id, reply client)  in
  Lwt_mvar.put message_box join_msg;
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
       Lwt_mvar.put message_box (Quit id);
      if String.length frame.content >= 2 then
        let content = String.sub frame.content 0 2 in
        Websocket_lwt.Connected_client.send client Websocket_lwt.Frame.(create ~opcode:Websocket_lwt.Frame.Opcode.Close ~content ())
      else
        Websocket_lwt.Connected_client.send client @@ Websocket_lwt.Frame.close 1000
    | Websocket_lwt.Frame.Opcode.Pong ->
      inner ()
    | Websocket_lwt.Frame.Opcode.Text ->
       Lwt_mvar.put message_box (Message (id, frame.content));
       inner ()
    | _ ->
       Lwt_mvar.put message_box (Quit id);
       let close_frame = Websocket_lwt.Frame.(close 1002) in
       Websocket_lwt.Connected_client.send client close_frame

  in inner ()


let run (uri : Uri.t) message_box =
  let open Lwt in
  let id = ref (-1) in
  let handle_conn client =
    incr id;
    let id = !id in
    Lwt.catch
      (fun () -> react client id message_box)
      (fun _ ->
        Lwt_mvar.put message_box (Quit id);
        Lwt_io.printf "error\n")
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
