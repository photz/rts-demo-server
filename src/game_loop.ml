let rec run gs =
  let open Lwt in
  let wait_s = 1. in
  Lwt_unix.sleep wait_s >>= fun () ->
  begin
    System.run gs wait_s;
    Lwt_io.printf "main loop just ran\n" >>= fun () -> 
    run gs
  end


  


