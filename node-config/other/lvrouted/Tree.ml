open Common

type node = {
	addr: Unix.inet_addr;
	mutable nodes: node list;
}

exception InvalidSignature

let make a = { addr = a; nodes = [] }

(* Traverse a list of nodes breadth-first, calling a function for every node. The
   callback function is fed three parameters: the gateway address to use, the parent
   node and the node itself. *)
let rec traverse f l = match l with
	  []		-> ()
	| (a,p,x)::xs	-> f x p a;
			   traverse f (List.append xs
			   		(List.map (fun node -> a, x, node) x.nodes))

(* Show the given list of nodes *)
let show l =
	let s = ref "" in
	let rec show' indent l =
		let i = String.make indent '\t' in
		List.iter (fun n ->
			s := !s ^ i ^ (Unix.string_of_inet_addr n.addr) ^ "\n";
			show' (indent + 1) n.nodes) l in
	show' 0 l;
	!s

(* Given a list of nodes and a set of our own addresses, return 
   a list of pruned nodes and a routing table.
   
   1. Initialize a routing table with routes to our own addresses.
   2. Make a temporary node to hook the list in, so the rest of
      the routine can assume to be working on a node structure.
   3. Traverse the tree breadth-first. For every node, check if
      there is a route already. If so, remove this node from
      the list of children in the parent. If not, add a route.

   To be able to do this, the callback to the traversal routine
   needs three pieces of information:
     - the node to work on
     - the parent of this node, to be able to remove this node
     - the gateway. The top of every node in the 'nodes'
       parameter is the gateway to the tree under it.
*)
let merge nodes (directips: (Unix.inet_addr * int) list) =
	let routes = ref (List.fold_left
				(fun map (a, _) -> IPMap.add a a map)
				IPMap.empty directips) in
	let fake = { addr = Unix.inet_addr_any; nodes = nodes } in
	traverse (fun node parent gw ->
			if IPMap.mem node.addr !routes then
			  parent.nodes <- List.filter (fun n -> n.addr != node.addr) parent.nodes
			else
			  routes := IPMap.add node.addr gw !routes)
		 (List.map (fun node -> node.addr, fake, node) nodes);
	routes := IPMap.fold (fun a gw map ->
			if List.exists (fun (a', n) ->
				Route.includes_impl a' n a 32) directips then map
			else IPMap.add a gw map) !routes IPMap.empty;
	fake.nodes, !routes

(* Send the given list of nodes over the given file descriptor to the
   given addr *)
let send (ts: node list) fd addr = 
	let s = Marshal.to_string ts [] in
	let s' = if Common.compress_data then LowLevel.string_compress s
		 else s in
	let s'' = if !Common.secret = "" then s'
		  else (LowLevel.sha_string (!Common.secret ^ s')) ^ s' in
	try ignore(Unix.sendto fd s'' 0 (String.length s'') []
			(Unix.ADDR_INET (addr, !Common.port)))
	with _ -> ()

(* Read a list of nodes from the given string and return a new node. Node as
   in tree node, not wireless network node. *)
let from_string s from_addr : node =
	let s' = if !Common.secret = "" then s
		 else let sha = String.sub s 0 20 in
		      let t = String.sub s 20 (String.length s - 20) in
		      if LowLevel.sha_string (!Common.secret ^ t) != sha then
		        raise InvalidSignature;
		      t in
	let s'' = if Common.compress_data then LowLevel.string_decompress s'
		  else s' in
	(* This is the most dangerous bit in all of the code: *)
	let nodes = (Marshal.from_string s'' 0: node list) in
	{ addr = from_addr; nodes = nodes }
