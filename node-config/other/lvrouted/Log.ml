let quiet = 0
let warnings = 1
let info = 2
let debug = 3

let loglevel = ref quiet
let logfile = ref stderr

let log level msg =
	if !loglevel >= level then begin
		if !logfile = stderr then
		  logfile := open_out "/tmp/lvrouted.log";
		output_string !logfile
			(string_of_int (int_of_float (Unix.time ())) ^ ": " ^
			 msg ^ "\n");
		flush !logfile
	end
