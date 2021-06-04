let to_8601 t =
  let open Unix in
  let t = gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (t.tm_year + 1900)
    (t.tm_mon + 1)
    (t.tm_mday)
    (t.tm_hour)
    (t.tm_min)
    (t.tm_sec)