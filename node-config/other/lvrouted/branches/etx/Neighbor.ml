(* Neighbor type definition, management and utility functions *)

type t = {
	iface: string;			(* "wi0", "ep0", etc *)
	addr: Unix.inet_addr;		(* address to reach this neighbor on *)
	mutable macaddr: MAC.t option;	(* MAC address, if known *)

	recvstamps: float Queue.t;
	mutable last_numpackets: int option;
	mutable etx: float;
	mutable tree: Tree.node option; (* the packet last received *)
}

type neighbor = t
module Set = Set.Make(struct
	type t = neighbor
	let compare a b = compare a.addr b.addr
end)

let name n = Unix.string_of_inet_addr n.addr
let show n = name n ^ " on " ^ n.iface ^ "\n"

(* constructor *)
let make iface addr =
	{ iface = iface;
	  addr = addr;
	  recvstamps = Queue.create ();
	  last_numpackets = None;
	  etx = 1.0;
	  macaddr = None;
	  tree = None }

let iface n = n.iface

(* send the given tree to the given neighbor *)
let send fd (nodes: Tree.node list) n =
	let packet = Packet.make (Queue.length n.recvstamps) nodes in
	try Packet.send packet fd n.addr
	with _ ->
		Log.log Log.info ("Nuking " ^ name n ^ "'s tree after exception while sending");
		n.tree <- None

(* Given a list of neighbors, data in a string and the sockaddr it came from,
   handle it. Find the neighbor associated with the address, parse the
   tree and mark the time *)
let handle_data ns s sockaddr =
	try	
		let addr = Common.get_addr_from_sockaddr sockaddr in
		let n = Set.filter (fun n -> n.addr = addr) ns in
		let n = List.hd (Set.elements n) in
		Log.log Log.debug ("This data is from neighbor " ^ name n);
		try
			let p = Packet.from_string s in
			n.last_numpackets <- Some (Packet.numreceived p);
			let t = Tree.make n.addr n.etx (Packet.nodes p) in
			n.tree <- Some t;
			Log.log Log.debug (name n ^ "'s tree has been set");
		with Packet.InvalidSignature ->
			Log.log Log.warnings
				("Received invalid signature from " ^ name n)
		   | _ ->
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

(* recalculate the etx score for the given neighbors *)
let recalc_etx ns =
	let now = Unix.gettimeofday () in
	let lower_limit = now -. Common.etx_length in
	Set.iter (fun n ->
		(* expire old timestamps *)
		while not (Queue.is_empty n.recvstamps) &&
		      Queue.peek n.recvstamps <= lower_limit do
			ignore(Queue.pop n.recvstamps)
		done;
		if Queue.length n.recvstamps = 0 ||
		   Common.is_none n.last_numpackets then begin
			n.last_numpackets <- None;
			n.etx <- 0.0
		end else begin
			let a = Common.from_some n.last_numpackets *
				Queue.length n.recvstamps in
			n.etx <- float_of_int a /.
				 Common.etx_length *. Common.etx_length
		end
	) ns

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
	end;
	reachable
