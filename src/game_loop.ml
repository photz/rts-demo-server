let rec run gs clients =
  let open Lwt in
  let wait_s = 1. in
  Lwt_unix.sleep wait_s >>= fun () ->
  begin
    System.run gs wait_s;
    let n = Core.Hashtbl.length clients in
    Lwt_io.printf "got %d clients\n" n >>= fun () ->
    Lwt_io.printf "main loop just ran\n" >>= fun () -> 
    run gs clients
  end


  


