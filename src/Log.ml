(* Simple logging functionality. should probably use syslog instead *)
let quiet = 0
let errors = 1
let warnings = 2
let info = 3
let debug = 4

let loglevel = ref quiet
let logfile = ref stderr

(* Log the given message with the given loglevel. If the current loglevel is
   higher or equal to the given level, the message is printed to the log. *)
let log level msg =
	if !loglevel >= level then
	  if !Common.use_syslog then
	    LowLevel.syslog level msg
	  else try
		if !logfile = stderr then
		  logfile := open_out "/tmp/lvrouted.log";
		output_string !logfile
			(string_of_int (int_of_float (Unix.time ())) ^ ": " ^
			 msg ^ "\n");
		flush !logfile
	  with _ -> ()


(* Given a loglevel and a function, if the current loglevel is higher or
   equal than the given level, execute the function. The function returns a
   list of strings, which then get printed to the log.

   This is to be used for messages that are relatively expensive to generate. *)
let lazylog level msgf =
	if !loglevel >= level then
	  List.iter (log level) (msgf ())
