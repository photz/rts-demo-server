let get_timestamp = fun () ->
  Core.Time_ns.now ()
  |> Core.Time_ns.to_int_ns_since_epoch
