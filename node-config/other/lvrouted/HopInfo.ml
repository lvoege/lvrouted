open Route

type t = {
	addr: Unix.inet_addr;
	path: Unix.inet_addr array;
}

type hopelts = t array

(* accessors *)
let addr e = e.addr

(* constructor *)
let make a = { addr = a; path = [| |] }

let hopcount h = Array.length h.path

let show h =
	Unix.string_of_inet_addr h.addr ^ " -> " ^
	string_of_int (hopcount h) ^ " hops"

let printelts hs =
	print_string ("Hoptable: ");
	print_newline ();
	Array.iter (fun e ->
		print_string ("\t" ^ show e);
		print_newline ()) hs

(* Send the given hoptable over the given file descriptor to the given addr *)
let send (hs:hopelts) fd addr = 
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
let from_string s from_addr : hopelts =
	let s' = if Common.compress_data then LowLevel.string_decompress s
		 else s in
	let res = (Marshal.from_string s' 0: hopelts) in
	Array.iteri (fun i e ->
		res.(i) <- { e with path = Array.append [| from_addr |] e.path }
	) res;
	res

let compare a b = compare (hopcount a) (hopcount b)

(* Filter all hopelements in hs that point to the given gateway per the
   given routing table. This prevents the count-to-infinity problem *)
let filter hs routes gw = 
	let f e =
	  try
		let r = Route.lookup routes e.addr != gw in
		r
	  with Not_found -> true in
	Array.of_list (List.filter f (Array.to_list hs))
let path_in_hash h addrhash =
	let res = ref false in
	Array.iter (fun a -> 
		try
			let _ = Hashtbl.find addrhash a in
			res := true
		with Not_found -> ()) h.path;
	!res
	
