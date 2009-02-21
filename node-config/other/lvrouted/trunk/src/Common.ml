(* This module contains tunables and convenience types and functions *)

(* unpack_string will verify the signature of a packet and will throw this
   exception if the signature turns out wrong *)
exception InvalidSignature

(*s Constants and tunables. Everything that is a ref here is settable
    through some commandline option. *)

(* The port to listen on *)
let port = ref 12345
(* How many seconds between advertising the hoptable to neighbors? *)
let bcast_interval = ref 30.0
(* How many seconds not hearing from a neighbor until considering it lost? *)
let timeout: float = 4.0 *. !bcast_interval
(* How many seconds between checking interfaces and expiry times? *)
let alarm_timeout = ref 9.0
(* Whether or not to compress the data that goes over the wire *)
let compress_data = false
(* Whether or not to really update the kernel's routing table *)
let real_route_updates = ref false
(* the netmask that's just narrow enough to be an interlink subnet.*)
let interlink_netmask = ref 28
(* at least how many seconds between updating interface association information? *)
let iface_assoc_update = 5.0
(* How many seconds between updates of the actual arp table in MAC.ml? *)
let arptables_update_every = 60.0
(* At least how many seconds between updating an iface's arp table?
   Note that MAC.ml also caches this, but Iface.ml post-processes the result,
   so this is really about how often to do the post-processing *)
let iface_arp_update = arptables_update_every
(* Secret "key". If not the empty string, this is appended to the marshalled
   trees, the whole thing is SHA1'ed and the hash appended. On receipt, the
   reverse is done and checked. Someone that doesn't know this key will not
   be able to produce a good hash. *)
let secret = ref ""
(* Whether or not to stay in the foreground or to daemon()ize *)
let foreground = ref false
(* Maximum number of route flush attempts *)
let max_route_flush_tries = 10
(* Log to syslog instead of /tmp/lvrouted.log *)
let use_syslog = ref false
(* What is the minimum (or widest) netmask Route.aggregate can produce? *)
let min_mask = ref 24
(* The IPv4 boundaries of the routable range. Min is inclusive, max is
   exclusive. *)
let min_routable = Unix.inet_addr_of_string "172.16.0.0"
let max_routable = Unix.inet_addr_of_string "172.31.255.0"
(* Use Tree.(de)serialize instead of the Marshal module. *)
let own_marshaller = true
(* Where to dump debug stuff and such *)
let tmpdir = ref "/tmp/"
(* An optional configuration file with extra addresses *)
let configfile = ref "/usr/local/etc/lvrouted.conf"

(* Types *)

module StringMap = Map.Make(String)
(* Define a struct that can be passed to the functorized Set, Map en Hashtbl modules *)
module IPStruct = struct
	(* the OrderedType signature *)
	type t = Unix.inet_addr
	let compare = LowLevel.compare_ipv4_addrs
	(* the HashedType signature *)
	let equal a b = LowLevel.compare_ipv4_addrs a b = 0
	let hash = Hashtbl.hash
end
module IPSet = Set.Make(IPStruct)
module IPMap = Map.Make(IPStruct)
module IPHash = Hashtbl.Make(IPStruct)

(* Convenience functions *)

(* Given an 'a option, it it a Some of a? *)
let is_some = function
	  None	-> false
	| _	-> true

(* or is it a None? *)
let is_none e = not (is_some e)

(* Given an 'a option that is a Some of a, return the a *)
let from_some = function
	  Some t -> t
	| _ -> raise (Failure "oops, from_some called on a None!")

(* Given a filtering function, a mapping function and a list, return
   the filtered-and-then-mapped list *)
let rec filtermap ff mf = function
	  []	-> []
	| x::xs	-> if ff x then (mf x)::(filtermap ff mf xs)
		   else filtermap ff mf xs

let get_addr_from_sockaddr = function
	  Unix.ADDR_UNIX _	-> raise (Failure "Huh, got a unix address?!")
	| Unix.ADDR_INET (a, _)	-> a

(* Given an open read channel, return a reversed (!) list of all lines *)
let snarf_lines_from_channel c =
	let res = ref [] in
	try
		while true do
			res := (input_line c)::(!res)
		done;
		!res
	with End_of_file ->
		!res

(* Given a channel, a regular expression and the number of groups to expect, return a list of
   arrays. The arrays are the matched groups. Note that the number of groups should include
   the 0th group, which is the entire matched string. So specify the number of parenthesis
   pairs plus one for the whole string. *)
let snarf_channel_for_re c re numgroups =
	let lines = snarf_lines_from_channel c in
	List.fold_left (fun acc l ->
		if Str.string_match re l 0 then begin
			let s = Str.matched_string l in
			let a = Array.init numgroups 
				(fun i -> Str.matched_group i s) in
			a::acc
		end else acc) [] lines

let read_file fname =
	let chan = open_in fname in
	let size = in_channel_length chan in
	let s = String.create size in
	let numread = input chan s 0 size in
	if size <> numread then String.sub s 0 numread
	else s

let sign_string s = 
	if !secret = "" then s
	else (LowLevel.sha_string (!secret ^ s)) ^ s

let verify_string s =
	if !secret = "" then true, s
	else let sha = String.sub s 0 20 in
	     let s' = String.sub s 20 (String.length s - 20) in
	     let sha' = LowLevel.sha_string (!secret ^ s') in
	     sha' = sha, s'

let try_max_times max f =
	let rec t i =
		if f i then true
		else if i = max then false
		else t (i + 1) in
	t 0

let addr_in_range a = a >= min_routable && a < max_routable

let pack_string s =
	let s = if compress_data then LowLevel.string_compress s
		else s in
	sign_string s

let unpack_string s =
	let goodsig, s = verify_string s in
	if not goodsig then
	  raise InvalidSignature;
	if compress_data then LowLevel.string_decompress s
	else s
