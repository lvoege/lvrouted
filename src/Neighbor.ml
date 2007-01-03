(* Neighbor type definition, management and utility functions *)

type neighbor = {
	iface: Iface.t option;
	mutable bandwidth: int;
	addr: Unix.inet_addr;		(* address to reach this neighbor on *)
	myaddr: Unix.inet_addr;
	mutable macaddr: MAC.t option;	(* MAC address, if known *)

	mutable last_seen: float;	(* -1.0 if never seen, else unix
					   timestamp of last received packet *)
	mutable seqno: int;		(* last seen sequence number *)
	mutable tree: Tree.node option; (* the tree last received *)
}

(* the exception handle_data will throw if given a faulty packet *)
exception InvalidPacket

module Set = Set.Make(struct
	type t = neighbor
	let compare a b = compare a.addr b.addr
end)

(* Constructors *)
let make iface addr myaddr =
	let bandwidth = if Iface.itype iface = Iface.WIRED then
				Iface.current_bandwidth iface
			else -1 in
	{ iface = Some iface; 
	  bandwidth = bandwidth;
	  addr = addr;
	  myaddr = myaddr;
	  last_seen = -1.0;
	  macaddr = None;
	  seqno = min_int;
	  tree = None }

(* Given that the Set above only cares about the addr field, this constructor
   sets up a struct just for use in Set operations. *)
let make_of_addr a = 
	{ iface = None;
	  bandwidth = -1;
	  addr = a;
	  myaddr = a;
	  last_seen = -1.0;
	  macaddr = None;
	  seqno = min_int;
	  tree = None }

let iface n = Common.from_some n.iface
let iname n = Iface.name (Common.from_some n.iface)
let name n = Unix.string_of_inet_addr n.addr

let show n = Unix.string_of_inet_addr n.addr ^ " on " ^
	     iname n ^ "\n"

(* Broadcast the given list of tree nodes to the given Set of neighbors over
   the given file descriptor. *)
let bcast fd nodes ns =
	let s = Tree.to_string nodes in
	let s = if Common.compress_data then LowLevel.string_compress s
		else s in
	Set.iter (fun n ->
		let now = LowLevel.pack_int (int_of_float (Unix.time ())) in
		let s = Common.sign_string (now ^ s) in
		try
			ignore(Unix.sendto fd s 0 (String.length s) []
				   (Unix.ADDR_INET (n.addr, !Common.port)))
		with _ -> ()) ns

(* Given a set of neighbors, data in a string and the sockaddr it came from,
   handle it. Verify the signature, find the neighbor associated with the
   address, verify the sequence number, parse the tree and mark the time.
   
   If we are a wifi master, we can't read the bandwidth to a neighbor
   directly, but neighbors can. So in that case, assume what the neighbor read
   as its bandwidth to us is accurate and simply copy it. *)
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
	bailwhen (stamp <= n.seqno)
		("Received old sequence number (" ^ string_of_int stamp ^
		 " <= " ^ string_of_int n.seqno ^ ") from");

	let s = String.sub s 4 (len - 4) in
	let s = if Common.compress_data then LowLevel.string_decompress s
		else s in

	Log.log Log.debug ("deserializing from " ^ addr_s);	
	begin try
		let node = Tree.from_string s addr n.bandwidth in

		if Iface.itype (Common.from_some n.iface) = Iface.WIFI_MASTER then begin
			(* Look for an edge to us in the new tree and copy the
			   bandwidth field. This edge is at depth 2. *)
			match Tree.find_node node 2 n.myaddr with
			| None -> Log.log Log.debug ("Couldn't find myself!")
			| Some myself ->
				Log.log Log.debug ("\tBandwidth: " ^ string_of_int (Tree.bandwidth myself));
				n.bandwidth <- Tree.bandwidth myself
		end else begin
		  Log.log Log.debug ("\tNot a master");
		end;

		let node = Tree.node_with_bandwidth node n.bandwidth in
		n.tree <- Some node;
	with Failure f ->
		Log.log Log.debug ("failed: " ^ f);	
		raise InvalidPacket
	end;
	n.seqno <- stamp;
	n.last_seen <- Unix.gettimeofday ();
	Log.log Log.debug (name n ^ "'s tree has been set")

(* Given a list of neighbors and interface i, invalidate the trees
   for all the neighbors on that interface *)
let nuke_trees_for_iface ns i =
	Log.log Log.debug ("nuking interface " ^ Iface.name i);
	Set.iter (fun n -> if Common.from_some n.iface = i then begin
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
	(* Fetch all valid trees from the neighbors *)
	let nodes = Common.filtermap (fun n -> Common.is_some n.tree)
			             (fun n -> Common.from_some n.tree) 
				     (Set.elements ns) in
	let message = List.fold_left (fun s a ->
			s ^ " " ^ (Unix.string_of_inet_addr (Tree.addr a))) "" nodes in
	Log.log Log.debug ("Available neighbors:" ^ message);
	(* Merge the trees into a new tree and an IPMap.t *)

(* To calculate a priority for a node we need to propagate some stuff through
   the tree. There are a couple of points that come into play:

   1) link bandwidths should influence the priority. it is better to route
      over fast (.11a) links than it is to route over slow (.11b) links, both
      for capacity and quality reasons.
   2) longer paths are worse than shorter paths, unless compensated enough by
      average bandwidth
   3) it follows that there is some number of .11a links that equal one .11b
      link as far as attractiveness goes. for example, we may say that a path
      with three 54MBps links is more attractive than one 11Mbps link, but
      that a path with four 54MBps links is /not/ more attractive than one
      11MBps link.
   4) ethernet is free as far as we're concerned here.
   5) bad links are to be avoided. use only when no other choice.

   Now the priority function gets the real depth of a node passed in. To
   implement 4), we propagate one of our own which we don't increment for
   ethernet links. We propagate the sum of the passed-in bandwidths and 
   use the real depth to calculate the average.
   
   Higher average bandwidth is good. Higher depth is not. So a good first stab
   is to divide the two, implementing points 1) and 2).

   Point 5 is implemented by propagating a negative score for paths with a bad
   link on them. This currently does not care about which link is less bad
   than another.
   
   Point 3 IS CURRENT UNIMPLEMENTED. it's probably some frobbing by scaling
   the amount 54Mbps is over 11Mbps and using the scaled factor, or something.
   *)
	let init_payload = Tree.bandwidth in
	let propagate payload n =
		if payload = -1 then -1		(* path with bad link *)
		else let bw = Tree.bandwidth n in
		     if bw == 1 || bw == 2 then -1 (* bad link *)
		     else if bw > 100 then payload + 100 (* future-proof cap *)
		     else payload + bw in
	let priority payload depth =
		if payload == -1 then -1.0
		else (float_of_int payload) /. (float_of_int (depth * depth * depth * depth)) in

	Log.log Log.debug ("Merging");
	let nodes', routemap = Tree.merge nodes
					  directips
					  propagate
					  priority
					  init_payload in
	Log.log Log.debug ("Merged");
	(* Fold the IPMap.t into a Route.Set.t *)
	let routeset =
		Common.IPHash.fold (fun addr gw ->
				     Route.Set.add (Route.make addr 32 gw))
				  routemap Route.Set.empty in
	Log.log Log.debug ("Made routeset");
	Route.aggregate routeset, nodes'

(* Check if the given neighbor is reachable over the given Iface.t. If it
   isn't, set the neighbor's tree to None. *)
let check_reachable n iface = 
	if Common.is_none n.macaddr then begin
		let arptable = MAC.get_arptable (iname n) in
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

let update_bandwidth n =
	let i = iface n in
	match Iface.itype i with
	| Iface.WIRED -> ()
	| Iface.WIFI_CLIENT -> n.bandwidth <- Iface.current_bandwidth i
	| Iface.WIFI_MASTER -> ()
