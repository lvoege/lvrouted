(* Neighbor type definition, management and utility functions *)

(* A neighbor has a name, an interface and address to reach it on, 
   a timestamp when we last successfully received something from the neighbor,
   and optionally the last hoptable received *)
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
module Map = Map.Make(NeighborType)

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

(* send the given hoptable to the given neighbor under the current routing
   table. hoptable entries going through the neighbor per the routing table
   will be filtered to counter the count-to-infinity problem *)
let send fd (routes: Route.Set.t) (nodes: Tree.node list) n =
	try Tree.send nodes fd n.addr;
	with _ -> ()

(* Given a list of neighbors, data in a string and the sockaddr it came from,
   handle it. Find the neighbor associated with the address, parse the
   hoptable and mark the time *)
let handle_data ns s sockaddr =
	try	
		let addr = Common.get_addr_from_sockaddr sockaddr in
		let n = List.find (fun n -> n.addr = addr) ns in
		Log.log Log.debug ("This data is from neighbor " ^ n.name);
		n.tree <- Some (Tree.from_string s addr);
		Log.log Log.debug (n.name ^ "'s hoptable has been set");
		n.last_seen <- Unix.gettimeofday ()
	with _ -> 
		Log.log Log.debug ("Cannot find neighbor for this data");
		()

(* Given a list of neighbors and interface i, invalidate the hoptables 
   for all the neighbors on that interface *)
let nuke_hoptable_for_iface ns i =
	Log.log Log.debug ("nuking interface " ^ i);
	List.iter (fun n -> if n.iface = i then begin
				n.tree <- None;
				Log.log Log.debug ("neighbor " ^ n.name ^ "canned")
			    end) ns

(* Given a list of neighbors and a number of seconds, invalidate the 
   hoptables of all neighbors not heard from since numsecs ago *)
let nuke_old_hoptables ns numsecs =
	let limit = (Unix.gettimeofday ()) -. numsecs in
	let res = ref false in
	List.iter (fun n -> 
		if n.last_seen < limit then begin
		  Log.log Log.debug (n.name ^ " expired");
		  n.tree <- None;
		  res := true
		end) ns;
	!res

(* From the given direct hoptable and list of neighbors, derive a list of
   (unaggregated) routes and a merged hoptable. *)
let derive_routes_and_hoptable directips ns = 
	let nodes = Common.filtermap (fun n -> Common.is_some n.tree)
			             (fun n -> Common.from_some n.tree) ns in
	Log.log Log.debug ("Number of eligible neighbors: " ^
			   string_of_int (List.length nodes));
	print_string ("before Tree.merge: \n" ^ Tree.show nodes);
	print_newline ();
	let nodes', routemap = Tree.merge nodes directips in
	print_string ("after Tree.merge: \n" ^ Tree.show nodes');
	print_newline ();
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
		Log.log Log.debug ("Setting " ^ n.name ^ "'s hoptable to None");
		n.tree <- None;
		n.last_seen <- 0.0
	end;
	reachable
