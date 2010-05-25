(* Simple logging functionality. should probably use syslog instead *)
let quiet = 0
let errors = 1
let warnings = 2
let info = 3
let debug = 4

let loglevel = ref quiet
let logfile = ref stderr

let reopen_log () = 
	(* Only log to file if we are in background *)
	if not(!Common.foreground) then begin
	  if !logfile != stderr then 
	    close_out !logfile;
	  logfile := open_out "/tmp/lvrouted.log"
	end

(* Log the given message with the given loglevel. If the current loglevel is
   higher or equal to the given level, the message is printed to the log. *)
let log level msg =
	if !loglevel >= level then begin
	  if !Common.use_syslog then
	    LowLevel.syslog level msg
	  else try
		(* Only log to file if we are in background *)
		if not(!Common.foreground) then 
		  if !logfile = stderr then
		    logfile := open_out "/tmp/lvrouted.log";
		let epoch = Unix.time () in 
		  output_string !logfile
		  	((Printf.sprintf "%.0f" epoch) ^ "[" ^ string_of_int level ^ "]: " ^
		  	 msg ^ "\n");
		flush !logfile
	  with _ -> ()
	end


(* Given a loglevel and a function, if the current loglevel is higher or
   equal than the given level, execute the function. The function returns a
   list of strings, which then get printed to the log.

   This is to be used for messages that are relatively expensive to generate. *)
let lazylog level msgf =
	if !loglevel >= level then
	  List.iter (log level) (msgf ())
