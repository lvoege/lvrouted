(* This module defines and implements the tree structure that is passed
   between nodes. It also implements the merging of trees, which is the
   core of the whole routing scheme. *)
open Common

type node = {
	addr: Unix.inet_addr;
	bandwidth: int;	(* The bandwidth from this node's parent to this
			   node, and presumably vice-versa, in megabits per
			   second *)
	mutable nodes: node list;
}

module FloatQueue = PrioQueue.Make(struct
	type t = float
	let compare = compare
end)

(* Constructors *)
let make a b nodes = { addr = a;
		       bandwidth = b;
		       nodes = nodes }
let make_direct a = { addr = a;
		      bandwidth = Common.infinite_bandwidth;
		      nodes = [] }

(* Accessors *)
let addr n = n.addr
let bandwidth n = n.bandwidth
let nodes n = n.nodes

(* Show the given list of nodes *)
let show l =
	let s = ref "" in
	let rec show' indent l =
		let i = String.make indent '\t' in
		List.iter (fun n ->
			s := !s ^ i ^ Unix.string_of_inet_addr n.addr ^ " @ " ^
				string_of_int n.bandwidth ^ "\n";
			show' (indent + 1) n.nodes) l in
	show' 0 l;
	!s

(* Given a list of nodes, put all addresses in those nodes and their
   descendants into an IPSet.t *)
let rec enumerate ns =
	List.fold_left (fun set n ->
		let set' = IPSet.add n.addr set in
		IPSet.union set' (enumerate n.nodes)) IPSet.empty ns

(*
let print_stuff depth node parent gw payload priority =
	let s = Unix.string_of_inet_addr (addr node) ^ ", depth=" ^
	string_of_int depth ^ ", parent=" ^ Unix.string_of_inet_addr (addr parent) ^
	", priority=" ^ (string_of_float (priority payload depth)) in
	s
*)

(* Given:
	- a list of spanning trees received from neighbors
	- a set of our own addresses
	- a function that can propagate a payload through the tree.
	  it will get the payload of the parent node plus the child node and
	  has to return the payload for that child node.
	- a function that returns a priority for a given payload at a given
	  depth
	- a function that can tell the payload for the top nodes.

   Return: the spanning tree for this node, plus a routing table.
   
   1. Initialize a routing table with routes to our own addresses.
   2. Make a new node to hang the new, merged and pruned tree under.
   3. Create an empty priority queue.
   3. Initialize the payloads for the top nodes, calculate their priorities
      and insert them into the priority queue.
   4. Traverse the tree according to the priority queue. For every node,
      check if there is a route already. If so, produce no new node. If not,
      add a route, create a new node and prepend it to the parent's
      list of children. Propagate the payload to the children, derive
      priorities for them and add them to the priority queue. Repeat until
      the queue is empty.
   4. Filter out routes that are included in a route from the
      list of directly attached routes. This may not be necessary anymore, but
      it was when route addition didn't work correctly.

   The traversal routine takes a routing table and a list of tuples to
   process. Those tuples are of the form
   	(depth, edge, parent, gateway address, payload).

   If the node on the edge isn't already in the routing table:
     - a new edge to a new node is produced and hooked under the parent
     - the new node is inserted into the routing table
     - the original node's children are pushed on the priority queue,
       with the new node as the parent. The gateway argument stays the same,
       the rest of the arguments are updated appropriately.

   Note that the resulting spanning tree is returned as the list of
   first-level nodes, because the top node is relevant only to the
   receiving end. The list of first-level nodes is sent to a neighbor,
   which will create a top node based on the address it received the
   packet from.
*)
let merge nodes		(* the list of nodes to merge *)
	  directnets	(* the list of directly attached nodes *)
	  propagate 	(* 'a -> node -> 'a: has to produce a new payload
	  		   given the old payload and a node. this is used to
			   propagate the payload through the tree *)
	  priority	(* 'a -> int -> float: has to produce the priority
	  			from the given payload at the given depth *)
	  init_payload	(* node -> 'a: given a top-level node, give the
	  			initial payload *)
	  =
	(* step 1 *)
	let routes = IPHash.create 512 in
	List.iter (fun (a, _) -> IPHash.add routes a a) directnets;
	(* step 2 *)
	let fake = make_direct Unix.inet_addr_any in
	(* step 3 *)
	let routes = IPHash.create 512 in
	let rec traverse queue =
		if queue = FloatQueue.empty then ()
		else	let (_, (depth, node, parent, gw, payload), queue') =
					FloatQueue.extract queue in
			if IPHash.mem routes node.addr then
			  traverse queue' (* ignore this node *)
			else begin
				(* copy this node and hook it into the new tree *)
				let newnode = { node with nodes = [] } in
				parent.nodes <- newnode::parent.nodes;
				(* push the children on the queue *)
				let queue'' = List.fold_left (fun queue n ->
					let payload' = propagate payload n in
					let c = (depth + 1, n, newnode, gw, payload') in
					let prio = priority payload' (depth + 1) in
					FloatQueue.insert queue prio c)
						queue' node.nodes in
				(* record the route *)
				IPHash.add routes node.addr gw;
				(* and continue traversing *)
				traverse queue''
			end in
	(* step 3: initialize the priority queue *)
	let todo = List.fold_left (fun queue n ->
		let payload = init_payload n in
		let c = 1, n, fake, n.addr, payload in
		FloatQueue.insert queue (priority payload 1) c)
			FloatQueue.empty nodes in
	traverse todo;
	(* step 4 *)
	IPHash.iter (fun a _ ->
		if List.exists (fun (a', n) ->
			LowLevel.route_includes_impl a' n a 32) directnets then
		  IPHash.remove routes a) routes;
	fake.nodes, routes

external serialize: node -> string = "tree_to_string"
external deserialize: string -> node = "string_to_tree"

let to_string (nodes: node list) =
	let fake = { addr = Unix.inet_addr_any; 
		     bandwidth = Common.infinite_bandwidth;
		     nodes = nodes } in
	if Common.own_marshaller then serialize fake 
	else Marshal.to_string nodes []

(* Read a list of nodes from the given string and return a new node. Node as
   in tree node, not wireless network node. *)
let from_string s from_addr bandwidth : node =
	if Common.own_marshaller then
	  { (deserialize s) with
	  	addr = from_addr;
		bandwidth = bandwidth }
	else
	  (* This is the most dangerous bit in all of the code: *)
	  { addr = from_addr;
	    bandwidth = bandwidth;
	    nodes = (Marshal.from_string s 0: node list) }

let dump_tree fname nodes =
	let out = open_out (!Common.tmpdir ^ fname) in
	output_string out (show nodes);
	close_out out

let find_parent node maxdepth addr =
	let rec traverse node depth =
		if node.addr = addr then Some node
		else if depth = maxdepth then None
		else List.fold_left (fun a n -> match a with
			| Some node -> Some node
			| None -> traverse n (depth + 1)
		) None node.nodes in
	traverse node 0

(* Neighbor.handle_data needs to be able to set the bandwidth after
   deserializing because the bandwidth may have to be read from the
   received tree. *)
let node_with_bandwidth n b = { n with bandwidth = b }
