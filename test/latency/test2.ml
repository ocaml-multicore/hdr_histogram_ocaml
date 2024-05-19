(** doesn't allocate *)
let mtime () = Monotonic_clock.now () |> Int64.to_int

let rec measure_self hdr n t0 t1 =
  if n > 0 then (
    let ok = Hdr_histogram.record_value hdr (t1 - t0) in
    assert ok;
    let t2 = mtime () in
    measure_self hdr (n - 1) t1 t2)

let () =
  let hdr =
    Hdr_histogram.init ~lowest_discernible_value:10
      ~highest_trackable_value:1_000_000 ~significant_figures:3
  in
  let t0 = mtime () in
  let t1 = mtime () in
  measure_self hdr 100_000 t0 t1;
  let ns_in_us = 1e3 in
  Hdr_histogram.hdr_percentiles_print hdr "/dev/stdout" 5l ns_in_us
    Hdr_histogram.CLASSIC;
  Hdr_histogram.close hdr
