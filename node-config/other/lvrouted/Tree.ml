open Common

type node = {
	addr: Unix.inet_addr;
	mutable nodes: node list;
}

let make a = { addr = a; nodes = [] }

(* Traverse a tree breadth-first, calling a function for every node *)
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
			s := !s ^ i ^ (Unix.string_of_inet_addr n.addr);
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
let merge nodes directips =
	let routes = ref (IPSet.fold (fun e map -> IPMap.add e e map)
				     directips IPMap.empty) in
	let fake = { addr = Unix.inet_addr_any; nodes = nodes } in
	traverse (fun node parent gw ->
			if IPMap.mem node.addr !routes then
			  parent.nodes <- List.filter ((!=) node) parent.nodes
			else
			  routes := IPMap.add node.addr gw !routes)
		 (List.map (fun node -> node.addr, fake, node) nodes);
(* TODO? directips er weer uit filteren? *)
	fake.nodes, !routes

(* Send the given list of nodes over the given file descriptor to the
   given addr *)
let send (ts: node list) fd addr = 
	let s = Marshal.to_string ts [] in
	let s' = if Common.compress_data then LowLevel.string_compress s
		 else s in
	try
		let _ = Unix.sendto fd s' 0 (String.length s) [] (Unix.ADDR_INET (addr, Common.port)) in
		()
	with Unix.Unix_error (e, _, _) ->
		prerr_string (Unix.error_message e);
		prerr_newline ()

(* Read a list of nodes from the given string and return a new node. *)
let from_string s from_addr : node =
	let s' = if Common.compress_data then LowLevel.string_decompress s
		 else s in
	(* This is the most dangerous bit in all of the code: *)
	let nodes = (Marshal.from_string s' 0: node list) in
	{ addr = from_addr; nodes = nodes }
