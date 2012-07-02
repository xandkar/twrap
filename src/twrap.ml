open Printf


let sep_date     = "-"
and sep_time     = ":"
and sep_datetime = " "
and sep_ts_data  = "==>"


let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  let year   = sprintf "%04d" (tm.Unix.tm_year + 1900)
  and month  = sprintf "%02d" (tm.Unix.tm_mon + 1)
  and day    = sprintf "%02d" tm.Unix.tm_mday
  and hour   = sprintf "%02d" tm.Unix.tm_hour
  and minute = sprintf "%02d" tm.Unix.tm_min
  and second = sprintf "%02d" tm.Unix.tm_sec
  in
  let date = String.concat sep_date [year; month; day]
  and time = String.concat sep_time [hour; minute; second]
  in
  date ^ sep_datetime ^ time


let rec print_stdin_lines () =
  try
    let timestamp = timestamp ()
    and line = read_line ()
    in
    let wrapped_line = sprintf "%s %s %s" timestamp sep_ts_data line in
    print_endline wrapped_line;
    print_stdin_lines ()
  with End_of_file ->
    ()


let main () =
  print_stdin_lines ()


let () = main ()
