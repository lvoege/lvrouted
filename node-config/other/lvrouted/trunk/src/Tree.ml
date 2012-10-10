(* This module defines and implements the tree structure that is passed
   between nodes. It also implements the merging of trees, which is the
   core of the whole routing scheme. *)
open Common

type node = {
	addr: Unix.inet_addr;
	eth: bool; 	(* Is this an ethernet link, but not one with
			   nanostations on it? *)
	gateway: bool; 	(* Is this a gateway to the outside? IOW, one that we
			   can lay a default route to? *)
	mutable nodes: node list;
}

module IntQueue = PrioQueue.Make(struct
	type t = int
	let compare = compare
end)

(* Constructor *)
let make a eth gateway nodes = { addr = a; eth = eth; gateway = gateway; nodes = nodes }

(* Accessors *)
let addr n = n.addr
let nodes n = n.nodes
let gateway n = n.gateway

(* For the tree topped by the given node, traverse it breadth-first, calling
   the given function on the nodes. If the function returns None, continue
   traversing. If it returns non-None, return the result as the result of the
   traversal. *)
let bfs node f =
	let rec traverse = function
		| [] -> None
		| x::xs -> match f x with
				| None -> traverse (xs@x.nodes)
				| Some r -> Some r in
	traverse [node]

(* Show the given list of nodes *)
let show l =
	let s = ref "" in
	let rec show' indent l =
		let i = String.make indent '\t' in
		List.iter (fun n ->
			s := !s ^ i ^ Unix.string_of_inet_addr n.addr ^ (if n.eth then " (eth)" else "") ^ (if n.gateway then " (gw)" else "") ^ "\n";
			show' (indent + 1) n.nodes) l in
	show' 0 l;
	!s

let dump_tree fname nodes =
	let out = open_out (!Common.tmpdir ^ fname) in
	output_string out (show nodes);
	close_out out

(* Given a list of spanning trees received from neighbors and a set of our
   own addresses, return the spanning tree for this node, plus a routing
   table.
   
   1. Initialize a routing table with routes to our own addresses. This is
      a mapping from address to a pair of cost and gateway.
   2. Reserve a slot to put the default gateway in. This would be the first
      address we see during the tree merge that is indeed marked as a valid
      gateway.
   3. Make a new node to hang the new, merged and pruned tree under.
   4. Traverse the tree breadth-first.  For every node, check if
      there is a route already. If there isn't, add a route and create a new
      node and prepend it to the parent's list of children.

      On the other hand, if there is already a route, check the cost (==
      priority) of that route. If it's less than the cost of the node we're
      looking at, ignore this node and continue traversing because we
      evidently already have a route to the node's address and it's strictly
      better. If it's more than the cost of the current node, panic because
      that shouldn't happen (that's the point of the priority queue after
      all).
      
      If it's equal, have the gateway that has the numerically lowest address
      of the two win. This will keep routes to addresses for which there are
      multiple equally costly paths stable. Also update the default gateway
      if necessary.
   5. From the routing table that maps addresses to pairs of cost and gateway,
      construct one that maps addresses to just the gateway, because the
      caller doesn't care about cost. While doing that, filter out routes that
      are included in a route from the list of directly attached routes. This
      may not be necessary anymore, but it was when route addition didn't work
      correctly.

   Note that the resulting spanning tree is returned as the list of
   first-level nodes, because the top node is relevant only to the
   receiving end. The list of first-level nodes is sent to a neighbor,
   which will create a top node based on the address it received the
   packet from.
*)
let merge nodes directnets =
	(* step 1 *)
	let routes = IPHash.create 512 in
	List.iter (fun (a, _) -> IPHash.add routes a (0, a)) directnets;
	(* step 2 *)
	let default_gw = ref Unix.inet_addr_any in
	(* step 3 *)
	let fake = make Unix.inet_addr_any true false [] in
	(* step 4 *)
	let rec traverse pq =
		if pq = IntQueue.empty then ()
		else    let (prio, (node, parent, gw), pq') = IntQueue.extract pq in
			if !default_gw = Unix.inet_addr_any && node.gateway then
			  default_gw := gw;
			try
				let (existing_cost, existing_gw) = IPHash.find routes node.addr in
				if prio == existing_cost && gw < existing_gw then begin
					IPHash.remove routes node.addr;
					IPHash.add routes node.addr (prio, gw);
					if !default_gw = existing_gw then
					  default_gw := gw;
				end;
				traverse pq'
			with Not_found -> 
				(* copy this node and hook it into the new tree *)
				let newnode = make node.addr node.eth node.gateway [] in
				parent.nodes <- newnode::parent.nodes;
				IPHash.add routes node.addr (prio, gw);

				(* Create queue elements for the children of
				   this node and push them on. For now the
				   priority is going to be the priority of the
				   parent plus one, giving normal BFS
				   behavior. *)
				let prio' = prio + (if node.eth then 1 else 10) in
				(*let prio' = prio + 1 in*)
				let pq'' = List.fold_left (fun pq' child ->
					let child_element = (child, newnode, gw) in
					IntQueue.insert pq' prio' child_element) pq' node.nodes in
				traverse pq'' in
	let todo = List.fold_left (fun q node ->
			let e = (node, fake, node.addr) in
			IntQueue.insert q 0 e) IntQueue.empty nodes in
	traverse todo;

	(* step 5 *)
	let routes' = IPHash.create 512 in
	IPHash.iter (fun a (_, gw) ->
		if not (List.exists (fun (a', n) ->
				LowLevel.route_includes_impl a' n a 32) directnets) then
		  IPHash.add routes' a gw) routes;
	fake.nodes, routes', !default_gw

external serialize: node -> string = "tree_to_string"
external deserialize: string -> node = "string_to_tree"

let to_string (nodes: node list) =
	let fake = { addr = Unix.inet_addr_any; eth = true; gateway = false; nodes = nodes } in
	if Common.own_marshaller then serialize fake 
	else Marshal.to_string nodes []

(* Read a list of nodes from the given string and return a new node. Node as
   in tree node, not wireless network node. *)
let from_string s from_addr : node =
	if Common.own_marshaller then
	  { (deserialize s) with addr = from_addr }
	else
	  (* This is the most dangerous bit in all of the code: *)
	  { addr = from_addr; eth = false; gateway = false; nodes = (Marshal.from_string s 0: node list) }
