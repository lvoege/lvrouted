(* This module defines and implements the tree structure that is passed
   between nodes. It also implements the merging of trees, which is the
   core of the whole routing scheme. *)
open Common

type edge = {
	edge_speed: int;
	edge_node: node;
} and node = {
	addr: Unix.inet_addr;
	mutable edges: edge list;
}

module IntQueue = PrioQueue.Make(struct 
	type priority = int
	let compare = compare
end)

(* Constructors *)
let make_edge s n = { edge_speed = s; edge_node = n }
let make_node a edges = { addr = a; edges = edges }

(* Accessors *)
let addr n = n.addr
let edges n = n.edges
let nodes n = List.map (fun e -> e.edge_node) n.edges

(* Show the given list of edges *)
let show l =
	let s = ref "" in
	let rec show' indent l =
		let i = String.make indent '\t' in
		List.iter (fun e ->
			let n = e.edge_node in
			s := !s ^ i ^ Unix.string_of_inet_addr n.addr ^ "\n";
			show' (indent + 1) n.edges) l in
	show' 0 l;
	!s

(* Given a list of spanning trees received from neighbors and a set of our
   own addresses, return the spanning tree for this node, plus a routing
   table.
   
   1. Initialize a routing table with routes to our own addresses.
   2. Make a new node to hang the new, merged and pruned tree under
   3. Traverse the tree according to a priority queue ordered according to
      the slowest link on the path.
   breadth-first.  For every node, check if
      there is a route already. If so, produce no new node. If not,
      add a route and create a new node and prepend it to the parent's
      list of children.
   4. Filter out routes that are included in a route from the
      list of directly attached routes. This may not be necessary anymore, but
      it was when route addition didn't work correctly.

   The traversal routine takes a routing table and a list of tuples to
   process. Those tuples are of the form (node, parent, gateway address).
   If the node isn't already in the routing table:
     - a new node is produced and hooked under the parent
     - the new node is inserted into the routing table
     - the original node's children are appended to the list of tuples to
       process, with the new node as the parent entry. the gateway argument
       stays the same.

   Note that the resulting spanning tree is returned as the list of
   first-level nodes, because the top node is relevant only to the
   receiving end. The list of first-level nodes is sent to a neighbor,
   which will create a top node based on the address it received the
   packet from.
*)
let merge edges directnets =
	(* step 1 *)
	let routes = List.fold_left (fun map (a, _) -> IPMap.add a a map)
				    IPMap.empty directnets in
	(* step 2 *)
	let fake = make_node Unix.inet_addr_any [] in
	(* step 3 *)
	let rec traverse routes queue =
		try
			let (mbps, (node, parent, gw), queue') =
				IntQueue.extract queue in
			if IPMap.mem node.addr routes then
			  traverse routes queue' (* ignore this node *)
			else begin
				(* copy this node and hook it into the new tree *)
				let newnode = make_node node.addr [] in
				parent.edges <-  { edge_speed = mbps;
						   edge_node = newnode}::parent.edges;
				(* push the children on the queue *)
				let queue'' = List.fold_left (fun queue e ->
					let mbps' = min mbps e.edge_speed in
					let c = (e.edge_node, newnode, gw) in
					IntQueue.insert queue mbps' c)
						IntQueue.empty node.edges in
				(* and continue traversing *)
				traverse (IPMap.add node.addr gw routes)
					 queue''
			end
		with IntQueue.Queue_is_empty -> routes in
	let todo = List.fold_left (fun queue e ->
		let c = e.edge_node, fake, e.edge_node.addr in
		IntQueue.insert queue e.edge_speed c) IntQueue.empty edges in
	let routes = traverse routes todo in
	(* step 4 *)
	let routes = IPMap.fold (fun a gw map ->
			if List.exists (fun (a', n) ->
				Route.includes_impl a' n a 32) directnets then map
			else IPMap.add a gw map) routes IPMap.empty in
	fake.edges, routes

external serialize: node -> string = "tree_to_string"
external deserialize: string -> node = "string_to_tree"

let to_string (edges: edge list) =
	let fake = { addr = Unix.inet_addr_any; edges = edges } in
	if Common.own_marshaller then serialize fake 
	else Marshal.to_string nodes []

(* Read a list of edges from the given string and return a new node. Node as
   in tree node, not wireless network node. *)
let from_string s from_addr : node =
	if Common.own_marshaller then
	  { (deserialize s) with addr = from_addr }
	else
	  (* This is the most dangerous bit in all of the code: *)
	  { addr = from_addr; edges = (Marshal.from_string s 0: edge list) }

let dump_tree fname nodes =
	let out = open_out (!Common.tmpdir ^ fname) in
	output_string out (show nodes);
	close_out out;
