(* Simple logging functionality. should probably use syslog instead *)
let quiet = 0
let errors = 1
let warnings = 2
let info = 3
let debug = 4

let loglevel = ref quiet
let logfile = ref stderr

let log level msg =
	if !loglevel >= level then
	  if !Common.use_syslog then
	    LowLevel.syslog level msg
	  else begin
		if !logfile = stderr then
		  logfile := open_out "/tmp/lvrouted.log";
		output_string !logfile
			(string_of_int (int_of_float (Unix.time ())) ^ ": " ^
			 msg ^ "\n");
		flush !logfile
	  end
