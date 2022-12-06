(lang dune 3.6)
(name get-activity)
(formatting disabled)
(generate_opam_files true)
(source (github talex5/get-activity))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name get-activity-bin)
 (synopsis "Collect activity as Markdown")
 (depends
  (cmdliner (>= 1.1.0))
  (get-activity (= :version))
  cohttp-lwt-unix))

(package
 (name get-activity)
 (synopsis "Collect activity as Markdown")
 (depends
  cohttp
  cohttp-lwt
  base-unix
  astring
  fmt
  yojson
  (ocaml (>= 4.08))))
