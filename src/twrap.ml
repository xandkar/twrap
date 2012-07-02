open Printf


let sep_date     = "-"
and sep_time     = ":"
and sep_datetime = " "
and sep_ts_data  = "==>"


let timestamp () =
  let open Unix in
  let tm = localtime (time ()) in
  let year   = sprintf "%04d" (tm.tm_year + 1900)
  and month  = sprintf "%02d" (tm.tm_mon + 1)
  and day    = sprintf "%02d" tm.tm_mday
  and hour   = sprintf "%02d" tm.tm_hour
  and minute = sprintf "%02d" tm.tm_min
  and second = sprintf "%02d" tm.tm_sec
  in
  let date = String.concat sep_date [year; month; day]
  and time = String.concat sep_time [hour; minute; second]
  in
  date ^ sep_datetime ^ time


let rec print_stdin_lines () =
    try
      printf "%s %s %s\n" (timestamp ()) sep_ts_data (read_line ());
      print_stdin_lines ()
    with End_of_file ->
      ()


let main () =
  print_stdin_lines ()


let () = main ()
