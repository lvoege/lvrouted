open Common

type t = {
	addr: Unix.inet_addr;
	mutable path: IPSet.t;
}

type hopelt = t
module Set = Set.Make(struct
	type t = hopelt
	let compare a b = compare a.addr b.addr
end)

(* accessors *)
let addr e = e.addr

(* constructor *)
let make a = { addr = a; path = IPSet.empty }

let hopcount h = IPSet.cardinal h.path

let show h =
	Unix.string_of_inet_addr h.addr ^ " -> " ^
	string_of_int (hopcount h) ^ " hops"

let printelts hs =
	print_string ("Hoptable: ");
	print_newline ();
	Set.iter (fun e ->
		print_string ("\t" ^ show e);
		print_newline ()) hs

(* Send the given hoptable over the given file descriptor to the given addr *)
let send (hs: Set.t) fd addr = 
	let s = Marshal.to_string hs [] in
	let s' = if Common.compress_data then LowLevel.string_compress s
		 else s in
	try
		let _ = Unix.sendto fd s' 0 (String.length s) [] (Unix.ADDR_INET (addr, Common.port)) in
		()
	with Unix.Unix_error (e, _, _) ->
		prerr_string (Unix.error_message e);
		prerr_newline ()

(* Read a hoptable from the given file descriptor. Will throw exceptions if
   the input is not a valid hoptable *)
let from_string s from_addr : Set.t =
	let s' = if Common.compress_data then LowLevel.string_decompress s
		 else s in
	(* This is the most dangerous bit in all of the code: *)
	let res = (Marshal.from_string s' 0: Set.t) in
	Set.iter (fun e ->
		e.path <- IPSet.add from_addr e.path) res;
	res

let compare a b = compare (hopcount a) (hopcount b)

(* Filter all hopelements in hs that point to the given gateway per the
   given routing table. This helps prevent the count-to-infinity problem. *)
let filter (hs: Set.t) (routes: Route.Set.t) gw = 
	Set.filter (fun e ->
	  try
		let r = Route.lookup routes e.addr != gw in
		r
	  with Not_found ->
	  	true) hs

(* are any of the path components in h present in addrhash? *)
let path_in_set (h: t) (addrset: IPSet.t) =
	not (IPSet.is_empty (IPSet.inter h.path addrset))
