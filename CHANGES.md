## Unreleased

## 0.0.6 (2026-09-09)

* Use correct `ar` binary target on Windows builds. (@ngorogiannis, #14)

## 0.0.5 (2026-04-29)

* Do not release runtime lock when recording values, ~1.8x faster value recording (@edwintorok, #9)
* Split functions that release the runtime lock into a separate module (@edwintorok, #10)
* Add binding to `hdr_percentiles_print` (@edwintorok, #10)
* Add self latency measurements (@edwintorok, #10)
* Avoid allocation in `Hdr_histogram.record_value` by using `int` instead of `int64` (@edwintorok, #10)
* Update repository name to `ocaml-multicore/hdr_histogram_ocaml` (@tmcgilchrist, #11)
* Add Windows support, not available on 32-bit bytecode platforms (@tmcgilchrist, #12)

## 0.0.4 (2024-01-09)

* Add FreeBSD support, fixes to endian includes for FreeBSD and DragonFly BSD (@tmcgilchrist, #7)

## 0.0.3 (2023-02-23)

* Add memory_size function (@crackcomm, #1)
* Change memory_size return type to int (@crackcomm, #2)
* Fix build with dune 3.6 (@emillon, #3)
* Remove ctypes include path hack by using ctypes 0.3 / dune 3.7 (@TheLortex, #5)

## 0.0.2 (2022-07-07)

* Moved repository
* Compile with fPIC

## 0.0.1 (2022-07-07)

* Initial release
