(* This module defines and implements the tree structure that is passed
   between nodes. It also implements the merging of trees, which is the
   core of the whole routing scheme. *)
open Common

type node = {
	addr: Unix.inet_addr;
	mutable nodes: node list;
}

(* Constructor *)
let make a nodes = { addr = a; nodes = nodes }

let rec copy t = { t with nodes = List.map copy t.nodes }

let addr n = n.addr
let nodes n = n.nodes

(* Show the given list of nodes *)
let show l =
	let s = ref "" in
	let rec show' indent l =
		let i = String.make indent '\t' in
		List.iter (fun n ->
			s := !s ^ i ^ Unix.string_of_inet_addr n.addr ^ "\n";
			show' (indent + 1) n.nodes) l in
	show' 0 l;
	!s

(* Given a list of spanning trees received from neighbors and a set of our
   own addresses, return the spanning tree for this node, plus a routing
   table.
   
   1. Initialize a routing table with routes to our own addresses.
   2. Make a new node to hang the new, merged and pruned tree under
   3. Traverse the tree breadth-first.  For every node, check if
      there is a route already. If so, produce no new node. If not,
      add a route and create a new node and prepend it to the parent's
      list of children.
   4. Filter out routes that are included in a route from the
      list of directly attached routes.

   The traversal routine applies a callback function to a list of
   (node, parent, gateway address) tuples. If the callback produces a
   new node, it is hooked under the parent's list of children and the
   original node's children are appended to the list of tuples to
   traverse.

TODO: 4 may be nothing more than cosmetics now that route addition
      finally works right. 4 was added because some of the evidence
      while debugging pointed to such routes acting up. check if it
      is just cosmetic now and note it.

   Note that the resulting spanning tree is returned as the list of
   first-level nodes, because the top node is relevant only to the
   receiving end. The list of first-level nodes is sent to a neighbor,
   which will create a top node based on the address it received the
   packet from.
*)
let merge nodes directnets =
	(* step 1*)
	let routes = List.fold_left
				(fun map (a, _) -> IPMap.add a a map)
				IPMap.empty directnets in
	(* step 2 *)
	let fake = make Unix.inet_addr_any [] in
	(* step 3 *)
	let rec traverse routes = function
		  []			-> routes
		| (node,parent,gw)::xs	-> 
			if IPMap.mem node.addr routes then
			  traverse routes xs
			else begin
				let newnode = make node.addr [] in
				parent.nodes <- newnode::parent.nodes;
				traverse (IPMap.add node.addr gw routes)
					 (xs@(List.map (fun node' -> node', newnode, gw) node.nodes))
			end in
	let routes = traverse routes (List.map (fun node -> node, fake, node.addr) nodes) in
	(* step 4 *)
	let routes = IPMap.fold (fun a gw map ->
			if List.exists (fun (a', n) ->
				Route.includes_impl a' n a 32) directnets then map
			else IPMap.add a gw map) routes IPMap.empty in
	fake.nodes, routes

external serialize: node -> string = "tree_to_string"
external deserialize: string -> node = "string_to_tree"

let to_string (nodes: node list) =
	let fake = { addr = Unix.inet_addr_any; nodes = nodes } in
	if Common.own_marshaller then serialize fake 
	else Marshal.to_string nodes []

(* Read a list of nodes from the given string and return a new node. Node as
   in tree node, not wireless network node. *)
let from_string s from_addr : node =
	if Common.own_marshaller then
	  { (deserialize s) with addr = from_addr }
	else
	  (* This is the most dangerous bit in all of the code: *)
	  { addr = from_addr; nodes = (Marshal.from_string s 0: node list) }

(* This is basically a hack. Given a list of first-level nodes and a set for
   which membership entails being connected to this node through an ethernet
   wire (as opposed to wireless), return a list of first-level nodes with
   the children of every wired neighbor promoted to direct neighbor. This
   then gets advertised and hides them from other neighbors, making the wired
   nodes essentially act as one. *)
let promote_wired_children wired nodes =
	let l' = List.map (fun n ->
				if IPSet.mem n.addr wired then
					let children = n.nodes in
					n.nodes <- [];
					n::children
				else [n]) nodes in
	List.concat l'

