(lang dune 2.3)
(name get-activity)
(formatting disabled)
(generate_opam_files true)
(source (github talex5/get-activity))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name get-activity-bin)
 (synopsis "collect activity as markdown")
 (depends
  get-activity
  cohttp-lwt-unix))

(package
 (name get-activity)
 (synopsis "collect activity as markdown")
 (depends
  cohttp
  cohttp-lwt
  unix
  astring
  fmt
  yojson
  (ocaml (>= 4.08))))
