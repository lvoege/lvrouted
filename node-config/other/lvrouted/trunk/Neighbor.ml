(* Neighbor type definition, management and utility functions *)

type t = {
	iface: string;			(* "wi0", "ep0", etc *)
	addr: Unix.inet_addr;		(* address to reach this neighbor on *)
	mutable macaddr: MAC.t option;	(* MAC address, if known *)

	mutable last_seen: float;	(* -1.0 if never seen, else unix
					   timestamp of last received packet *)
	mutable seqno: int;
	mutable tree: Tree.node option; (* the tree last received *)
}

exception InvalidPacket

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
	  last_seen = -1.0;
	  macaddr = None;
	  seqno = 0;
	  tree = None }

let iface n = n.iface

(* Broadcast the given list of tree nodes to the given Set of neighbors over
   the given file descriptor. *)
let bcast fd nodes ns =
	let s = Tree.to_string nodes in
	let s = if Common.compress_data then LowLevel.string_compress s
		else s in
	Set.iter (fun n ->
		let now = LowLevel.pack_int (int_of_float (Unix.time ())) in
		let s' = Common.sign_string (now ^ s) in
		try
			ignore(Unix.sendto fd s' 0 (String.length s') []
				   (Unix.ADDR_INET (n.addr, !Common.port)))
		with _ -> ()) ns

(* Given a set of neighbors, data in a string and the sockaddr it came from,
   handle it. Find the neighbor associated with the address, parse the
   tree and mark the time. *)
let handle_data ns s sockaddr =
	let addr = Common.get_addr_from_sockaddr sockaddr in
	let addr_s = Unix.string_of_inet_addr addr in
	let bailwhen c s =
		if c then begin
			Log.log Log.info (s ^ " " ^ addr_s);
			raise InvalidPacket
		end in

	let goodsig, s = Common.verify_string s in
	bailwhen (not goodsig) "Received invalid signature from";

        let len = String.length s in
	bailwhen (len < 4) "Received short packet from";

	let n = Set.filter (fun n -> n.addr = addr) ns in
	bailwhen (Set.is_empty n) "Cannot find neighbor with address";

	let n = List.hd (Set.elements n) in
	let stamp = LowLevel.unpack_int (String.sub s 0 4) in
	bailwhen (stamp <= n.seqno) "Received old sequence number from";

	let s = String.sub s 4 (len - 4) in
	let s = if Common.compress_data then LowLevel.string_decompress s
		else s in
	let nodes = (Marshal.from_string s 0: (Tree.node list)) in
	n.tree <- Some (Tree.make addr nodes);
	n.seqno <- stamp;
	n.last_seen <- Unix.gettimeofday ();
	Log.log Log.debug (name n ^ "'s tree has been set")

(* Given a list of neighbors and interface i, invalidate the trees
   for all the neighbors on that interface *)
let nuke_trees_for_iface ns i =
	Log.log Log.debug ("nuking interface " ^ i);
	List.iter (fun n -> if n.iface = i then begin
				n.tree <- None;
				Log.log Log.debug ("neighbor " ^ name n ^ " canned")
			    end) ns

(* Given a list of neighbors and a number of seconds, invalidate the 
   trees of all neighbors not heard from since numsecs ago *)
let nuke_old_trees ns numsecs =
	let limit = (Unix.gettimeofday ()) -. numsecs in
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
		let arptable = MAC.get_arptable n.iface in
		try  n.macaddr <- Some (Common.IPMap.find n.addr arptable)
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
