(* Neighbor type definition, management and utility functions *)

type t = {
	iface: string;			(* "wi0", "ep0", etc *)
	addr: Unix.inet_addr;		(* address to reach this neighbor on *)
	mutable macaddr: MAC.t option;	(* MAC address, if known *)

	recvstamps: float Queue.t;
	mutable last_seen: float;
	mutable tree: Tree.node option; (* the tree last received *)
}

type neighbor = t
module Set = Set.Make(struct
	type t = neighbor
	let compare a b = compare a.addr b.addr
end)

let show n = Unix.string_of_inet_addr n.addr ^ " on " ^ n.iface ^ "\n"
let name n = Unix.string_of_inet_addr n.addr

(* constructor *)
let make iface addr =
	{ iface = iface;
	  addr = addr;
	  recvstamps = Queue.create ();
	  last_seen = -1.0;
	  macaddr = None;
	  tree = None }

let iface n = n.iface

(* send the given tree to the given neighbor *)
let send fd (nodes: Tree.node list) n =
	try
		let s = Marshal.to_string (char_of_int (Queue.length n.recvstamps), nodes) [] in
		let s' = if Common.compress_data then LowLevel.string_compress s
			 else s in
		let s'' = Common.sign_string s' in
		ignore(Unix.sendto fd s'' 0 (String.length s'') []
				   (Unix.ADDR_INET (n.addr, !Common.port)))
	with _ ->
		Log.log Log.info ("Nuking " ^ name n ^ "'s tree after exception while sending");
		n.tree <- None

(* Given a list of neighbors, data in a string and the sockaddr it came from,
   handle it. Find the neighbor associated with the address, parse the
   tree and mark the time *)
let handle_data ns s sockaddr =
	let addr = Common.get_addr_from_sockaddr sockaddr in
	let goodsig, s' = Common.verify_string s in
	if not goodsig then
		Log.log Log.warnings
			("Received invalid signature from " ^ Unix.string_of_inet_addr addr)
	else try
		let s'' = if Common.compress_data then LowLevel.string_decompress s'
			  else s' in
		let n = Set.filter (fun n -> n.addr = addr) ns in
		let n = List.hd (Set.elements n) in
		Log.log Log.debug ("This data is from neighbor " ^ name n);
		try
			let numreceived, nodes = (Marshal.from_string s'' 0: (char * (Tree.node list))) in
			let numreceived = int_of_char numreceived in
			if numreceived < Queue.length n.recvstamps - Common.max_lost_packets then
			  Log.log Log.warnings (name n ^ "'s tree has been ignored because of asymmetry")
			else begin
				n.tree <- Some (Tree.make addr nodes);
				Log.log Log.debug (name n ^ "'s tree has been set");
				n.last_seen <- Unix.gettimeofday ();
				Queue.push n.last_seen n.recvstamps
			end
		with _ ->
			Log.log Log.warnings
				("Received invalid packet from " ^ name n)
	with _ -> 
		Log.log Log.debug ("Cannot find neighbor for this data")

(* Given a list of neighbors and interface i, invalidate the trees
   for all the neighbors on that interface *)
let nuke_trees_for_iface ns i =
	Log.log Log.debug ("nuking interface " ^ i);
	List.iter (fun n -> if n.iface = i then begin
				n.tree <- None;
				Log.log Log.debug ("neighbor " ^ name n ^ " canned")
			    end) ns

(* Given a list of neighbors and a number of seconds, invalidate the 
   trees of all neighbors not heard from since numsecs ago and trim the recvstamps queues  *)
let nuke_old_stuff ns numsecs =
	let limit = (Unix.gettimeofday ()) -. numsecs in
	(* expire old timestamps for all neighbors *)
	Set.iter (fun n ->
		while not (Queue.is_empty n.recvstamps) && 
		      Queue.peek n.recvstamps <= limit do
			ignore(Queue.pop n.recvstamps)
		done) ns;
	(* now nuke trees of neighbors not heard of since the limit *)
	let expired = Set.filter (fun n ->
		n.last_seen < limit && Common.is_some n.tree) ns in
	Set.iter (fun n ->
		Log.log Log.debug (name n ^ " expired");
		n.tree <- None) expired;
	not (Set.is_empty expired)

(* From the given set of direct IPs and list of neighbors, derive a list of
   (unaggregated) routes and a merged tree. *)
let derive_routes_and_mytree directips ns = 
	let nodes = Common.filtermap (fun n -> Common.is_some n.tree)
			             (fun n -> Common.from_some n.tree) 
				     (Set.elements ns) in
	Log.log Log.debug ("Number of eligible neighbors: " ^
			   string_of_int (List.length nodes));
	let nodes', routemap = Tree.merge nodes directips in
	let routeset = Common.IPMap.fold (fun addr gw set ->
				     Route.Set.add (Route.make addr 32 gw) set)
				  routemap Route.Set.empty in
	Route.aggregate routeset, nodes'

let check_reachable n iface = 
	if Common.is_none n.macaddr then begin
		let arptable = MAC.arptable n.iface in
		try  n.macaddr <- Some (Hashtbl.find arptable n.addr)
		with Not_found ->
			Log.log Log.debug ("Cannot determine MAC address for " ^
					   "neighbor " ^ name n);
	end;
	let reachable = Common.is_some n.macaddr &&
			Iface.is_reachable iface (Common.from_some n.macaddr) in
	if not reachable then begin
		Log.log Log.debug ("Setting " ^ name n ^ "'s tree to None");
		n.tree <- None;
		n.last_seen <- 0.0
	end;
	reachable
