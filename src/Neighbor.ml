(* Neighbor type definition, management and utility functions *)

type neighbor = {
	iface: string;			(* "wi0", "ep0", etc *)
	addr: Unix.inet_addr;		(* address to reach this neighbor on *)
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

(* constructor *)
let make iface addr =
	{ iface = iface;
	  addr = addr;
	  last_seen = -1.0;
	  macaddr = None;
	  seqno = min_int;
	  tree = None }

let iface n = n.iface
let name n = Unix.string_of_inet_addr n.addr

let show n = Unix.string_of_inet_addr n.addr ^ " on " ^ n.iface ^ "\n"

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
   address, verify the sequence number, parse the tree and mark the time. *)
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
		n.tree <- Some (Tree.from_string s addr);
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
	Log.log Log.debug ("nuking interface " ^ i);
	Set.iter (fun n -> if n.iface = i then begin
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
let derive_routes_and_mytree directips ns default_addrs = 
	(* Fetch all valid trees from the neighbors *)
	let nodes = Common.filtermap (fun n -> Common.is_some n.tree)
			             (fun n -> Common.from_some n.tree) 
				     (Set.elements ns) in
	Log.log Log.debug ("Number of eligible neighbors: " ^
			   string_of_int (List.length nodes));
	(* Merge the trees into a new tree and an IPMap.t *)
	let nodes', routemap = Tree.merge nodes directips in

	(* Fold the IPMap.t into a Route.Set.t *)
	let routeset =
		Common.IPHash.fold (fun addr gw ->
				     Route.Set.add (Route.make addr 32 gw))
				  routemap Route.Set.empty in
	(* Insert a default route to the nearest entry in default_addrs, if
	   any *)
	let look_for_default_addr n = 
		let a = Tree.addr n in
		match Common.IPSet.mem a default_addrs with
		| true -> Some a
		| false -> None in
	let bogus_top_node = Tree.make (Unix.inet_addr_of_string "255.255.255.255") nodes' in
	let first_default_addr = Tree.bfs bogus_top_node look_for_default_addr in
	let default_route = match first_default_addr with
		| None -> None
		| Some a -> (
			Log.log Log.debug ("First default addr: " ^ (Unix.string_of_inet_addr a));
			let gw = List.fold_left (fun acc (addr, mask) -> match acc with
				| Some x -> Some x
				| None -> if LowLevel.route_includes_impl addr mask a 32 then
						Some a
					  else
					  	None) None directips in
			let gw = match gw with
				| None -> (try Some (Common.IPHash.find routemap a)
					   with Not_found -> None)
				| Some x -> Some x in
			match gw with
			| None -> Log.log Log.errors ("Eek! Found a first default addr, but don't have a route to it!");
				  None
			| Some gw -> (
				Log.log Log.debug ("Gateway: " ^ (Unix.string_of_inet_addr gw));
				let a' = Unix.inet_addr_of_string "0.0.0.0" in
				Some (Route.make a' 0 gw)
			)
		) in
	Log.log Log.debug ("Done default addrs");

	let routeset' = Route.aggregate routeset in
	let routeset'' = match default_route with
		| None -> routeset'
		| Some r -> Route.Set.add r routeset' in

	routeset'', nodes'

(* Check if the given neighbor is reachable over the given Iface.t. If it
   isn't, set the neighbor's tree to None. *)
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
