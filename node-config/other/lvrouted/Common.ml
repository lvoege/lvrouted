(* This module contains useful little things *)

(*s Constants *)

(* The port to listen on *)
let port = 12345
(* How many seconds between advertising the hoptable to neighbors? *)
let bcast_interval: float = 5.0
(* How many seconds not hearing from a neighbor until considering it lost? *)
let timeout: float = 2.0 *. bcast_interval
(* How many seconds between checking interfaces and expiry times? *)
let alarm_timeout: int = 1

(* Convenience functions *)

(* Given an 'a option, it it a Some of a? *)
let is_some e = match e with
	  None -> false
	| _ -> true

(* or is it a None? *)
let is_none e = not (is_some e)

(* Given an 'a option that is a Some of a, return the a *)
let from_some e = match e with
	  Some t -> t
	| _ -> raise (Failure "oops, from_some called on a None!")

(* Given a filtering function, a mapping function and a list, return
   the filtered-and-then-mapped list *)
let rec filtermap ff mf l = match l with
	  []	-> []
	| x::xs	-> if ff x then ((mf x)::(filtermap ff mf xs))
		   else filtermap ff mf xs

let get_addr_from_sockaddr sockaddr =
	match sockaddr with
	  Unix.ADDR_UNIX _	-> raise (Failure "Huh, got a unix address?!")
	| Unix.ADDR_INET (a, _)	-> a
