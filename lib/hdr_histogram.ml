open Ctypes
module Types = Types_generated

type t = Types.hdr_histogram structure ptr

let init ~lowest_discernible_value ~highest_trackable_value ~significant_figures
    =
  let h =
    allocate (ptr Types.hdr_histogram) (from_voidp Types.hdr_histogram null)
  in
  let res =
    C.Function.hdr_init
      (Int64.of_int lowest_discernible_value)
      (Int64.of_int highest_trackable_value)
      significant_figures h
  in
  assert (res = 0);
  let h' : t = !@h in
  h'

let record_value h v = C.Function_fast.hdr_record_value_int h v
let close h = C.Function.hdr_close h

let value_at_percentile h p =
  Int64.to_int @@ C.Function.hdr_value_at_percentile h p

let max h = Int64.to_int @@ C.Function_fast.hdr_max h
let min h = Int64.to_int @@ C.Function_fast.hdr_min h
let mean h = C.Function_fast.hdr_mean h
let stddev h = C.Function_fast.hdr_stddev h

let memory_size h =
  Unsigned.Size_t.to_int @@ C.Function_fast.hdr_get_memory_size h

module Stdio = struct
  let raise_unix_error err fn arg =
    let err = Signed.SInt.to_int err in
    raise (Unix.Unix_error (C.Stdio.unix_error_of_code err, fn, arg))

  let fopen name mode =
    let file, errno = C.Stdio.fopen name mode in
    if is_null file then raise_unix_error errno "fopen" name;
    file

  let fclose file =
    let rc, errno = C.Stdio.fclose file in
    if rc <> 0 then raise_unix_error errno "fclose" ""

  let with_file name mode f =
    let file = fopen name mode in
    match f file with
    | r ->
        fclose file;
        r
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        let () = try fclose file with _ -> () in
        Printexc.raise_with_backtrace e bt
end

type format_type = Types.format_type = CLASSIC | CSV

let hdr_percentiles_print hdr filename ticks_per_half_distance value_scale
    format =
  Stdio.with_file filename "w" @@ fun file ->
  let errno =
    C.Function.hdr_percentiles_print hdr file ticks_per_half_distance
      value_scale format
  in
  (* returns EIO on error *)
  if errno <> 0 then
    Stdio.raise_unix_error (Signed.SInt.of_int errno) "hdr_percentiles_print"
      filename
