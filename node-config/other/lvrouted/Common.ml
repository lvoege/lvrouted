(* This module contains useful little things *)

(*s Constants *)

(* The port to listen on *)
let port = 12345
(* How many seconds between advertising the hoptable to neighbors? *)
let bcast_interval: float = 30.0
(* How many seconds not hearing from a neighbor until considering it lost? *)
let timeout: float = 2.0 *. bcast_interval
(* How many seconds between checking interfaces and expiry times? *)
let alarm_timeout: int = 9
(* Whether or not to compress the data that goes over the wire *)
let compress_data = false
(* Whether or not to really update the kernel's routing table *)
let real_route_updates = false
(* the netmask that's just narrow enough to be an interlink subnet.*)
let interlink_netmask = 28
(* at least how many seconds between updating interface association information? *)
let iface_assoc_update = 5.0
(* How many seconds between updates of the actual arp table in MAC.ml? *)
let arptables_update_every = 60.0
(* At least how many seconds between updating an iface's arp table?
   Note that MAC.ml also caches this, but Iface.ml post-processes the result,
   so this is really about how often to do the post-processing *)
let iface_arp_update = arptables_update_every

(* Types *)

module StringMap = Map.Make(String)
module IPStruct = struct
	type t = Unix.inet_addr
	let compare a b = compare a b
end
module IPSet = Set.Make(IPStruct)
module IPMap = Map.Make(IPStruct)

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

let snarf_lines_from_channel c =
	let res = ref [] in
	try
		while true do
			res := (input_line c)::(!res)
		done;
		!res
	with End_of_file ->
		List.rev !res

let snarf_channel_for_re c re numgroups =
	let lines = snarf_lines_from_channel c in
	let res = ref [] in
	List.iter (fun l ->
		if Str.string_match re l 0 then begin
			let s = Str.matched_string l in
			let a = Array.init numgroups 
				(fun i -> Str.matched_group i s) in
			res := a::(!res)
		end
	) lines;
	List.rev (!res)
