open Ctypes

module Types (F : Ctypes.TYPE) = struct
  (*   open F *)

  type hdr_histogram

  let hdr_histogram : hdr_histogram structure typ = structure "hdr_histogram"
  let classic = F.constant "CLASSIC" F.int64_t
  let csv = F.constant "CSV" F.int64_t
  let x = F.typedef

  type format_type = CLASSIC | CSV

  let format_type =
    F.enum ~typedef:true "format_type" [ (CLASSIC, classic); (CSV, csv) ]

  type file

  let file : file structure typ = typedef (structure "") "FILE"
end
