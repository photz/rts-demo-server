let ns_to_s ns =
  let ns_per_s = 1_000_000_000. in
  (Core.Float.of_int ns) /. ns_per_s

let get_timestamp = fun () ->
  Core.Time_ns.now ()
  |> Core.Time_ns.to_int_ns_since_epoch
