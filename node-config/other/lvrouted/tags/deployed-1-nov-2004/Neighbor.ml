(* Neighbor type definition, management and utility functions *)

(* A neighbor has a name, an interface and address to reach it on, 
   a timestamp when we last successfully received something from the neighbor,
   and optionally the last tree received *)
type t = {
	name: string;
	iface: string;
	addr: Unix.inet_addr;
	mutable macaddr: MAC.t option;

	mutable last_seen: float;
	mutable tree: Tree.node option;
}

type neighbor = t
module NeighborType = struct
	type t = neighbor
	let compare a b = compare a.addr b.addr
end
module Set = Set.Make(NeighborType)

let show n =
	n.name ^ ": " ^ Unix.string_of_inet_addr n.addr ^ " on " ^
			n.iface ^ "\n"

(* constructor *)
let make name iface addr =
	{ name = name; iface = iface; addr = addr;
	  last_seen = -1.0;
	  macaddr = None;
	  tree = None }

let name n = n.name
let iface n = n.iface

(* send the given tree to the given neighbor *)
let send fd (nodes: Tree.node list) n =
	try Tree.send nodes fd n.addr
	with _ ->
		Log.log Log.info ("Nuking " ^ n.name ^ "'s tree after exception while sending");
		n.tree <- None

(* Given a list of neighbors, data in a string and the sockaddr it came from,
   handle it. Find the neighbor associated with the address, parse the
   tree and mark the time *)
let handle_data ns s sockaddr =
	try	
		let addr = Common.get_addr_from_sockaddr sockaddr in
		let n = Set.filter (fun n -> n.addr = addr) ns in
		let n = List.hd (Set.elements n) in
		Log.log Log.debug ("This data is from neighbor " ^ n.name);
		try
			n.tree <- Some (Tree.from_string s addr);
			Log.log Log.debug (n.name ^ "'s tree has been set");
			n.last_seen <- Unix.gettimeofday ()
		with Tree.InvalidSignature ->
			Log.log Log.warnings
				("Received invalid signature from " ^ n.name)
	with _ -> 
		Log.log Log.debug ("Cannot find neighbor for this data")

(* Given a list of neighbors and interface i, invalidate the trees
   for all the neighbors on that interface *)
let nuke_trees_for_iface ns i =
	Log.log Log.debug ("nuking interface " ^ i);
	List.iter (fun n -> if n.iface = i then begin
				n.tree <- None;
				Log.log Log.debug ("neighbor " ^ n.name ^ "canned")
			    end) ns

(* Given a list of neighbors and a number of seconds, invalidate the 
   trees of all neighbors not heard from since numsecs ago *)
let nuke_old_trees ns numsecs =
	let limit = (Unix.gettimeofday ()) -. numsecs in
	let res = ref false in
	Set.iter (fun n -> 
		if n.last_seen < limit &&
		   Common.is_some n.tree then begin
			Log.log Log.debug (n.name ^ " expired");
			n.tree <- None;
			res := true
		end) ns;
	!res

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
		with Not_found -> ()
	end;
	if Common.is_none n.macaddr then
	  Log.log Log.debug ("Cannot determine MAC address for neighbor " ^ n.name);
	let reachable = (Common.is_some n.macaddr) &&
			(Iface.is_reachable iface (Common.from_some n.macaddr)) in
	if not reachable then begin
		Log.log Log.debug ("Setting " ^ n.name ^ "'s tree to None");
		n.tree <- None;
		n.last_seen <- 0.0
	end;
	reachable
