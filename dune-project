(lang dune 3.3)
(using ctypes 0.1)

(name hdr_histogram)

(generate_opam_files true)

(source
 (github ocaml-multicore/hdr_histogram_ocaml))

(authors "KC Sivaramakrishnan")

(maintainers "KC Sivaramakrishnan" "Christiano Haesbaert")

(license MIT)

(documentation https://ocaml-multicore.github.io/hdr_histogram_ocaml)

(package
 (name hdr_histogram)
 (synopsis "OCaml bindings to Hdr Histogram")
 (description "OCaml bindings to Hdr Histogram")
 (depends ocaml dune (ctypes (>= 0.20.1)) (ctypes-foreign (>= 0.18.0))
          (conf-pkg-config :build))
 (tags
  ("histogram" "tail latency")))