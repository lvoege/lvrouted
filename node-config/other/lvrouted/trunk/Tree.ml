(* This module defines and implements the tree structure that is passed
   between nodes. It also implements the merging of trees, which is the
   core of the whole routing scheme. *)
open Common

type node = {
	addr: Unix.inet_addr;
	mutable nodes: node list;
}

(* Tree.from_string will verify the signature of a packet and will throw this
   exception if the signature turns out wrong *)
exception InvalidSignature

(* Constructor *)
let make a = { addr = a; nodes = [] }

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

(* Given a list of nodes and a set of our own addresses, return 
   a list of new, pruned nodes and a routing table.
   
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
*)
let merge nodes directnets =
	(* step 1*)
	let routes = List.fold_left
				(fun map (a, _) -> IPMap.add a a map)
				IPMap.empty directnets in
	(* step 2 *)
	let fake = { addr = Unix.inet_addr_any; nodes = nodes } in
	(* step 3 *)
	let rec traverse routes = function
		  []			-> routes
		| (node,p,gw)::xs	-> 
			if IPMap.mem node.addr routes then
			  traverse routes xs
			else begin
				p.nodes <- { addr = node.addr; nodes = []}::p.nodes;
				traverse (IPMap.add node.addr gw routes)
					 (xs@(List.map (fun node' -> node', node, gw) node.nodes))
			end in
	let routes = traverse routes (List.map (fun node -> node, fake, node.addr) nodes) in
	(* step 4 *)
	let routes = IPMap.fold (fun a gw map ->
			if List.exists (fun (a', n) ->
				Route.includes_impl a' n a 32) directnets then map
			else IPMap.add a gw map) routes IPMap.empty in
	fake.nodes, routes

let to_string (nodes: node list) =
	let s = Marshal.to_string nodes [] in
	let s = if Common.compress_data then LowLevel.string_compress s
		else s in
	Common.sign_string s

(* Read a list of nodes from the given string and return a new node. Node as
   in tree node, not wireless network node. *)
let from_string s from_addr : node =
	let goodsig, s' = Common.verify_string s in
	if not goodsig then
	  raise InvalidSignature;
	let s'' = if Common.compress_data then LowLevel.string_decompress s'
		  else s' in
	(* This is the most dangerous bit in all of the code: *)
	let nodes = (Marshal.from_string s'' 0: node list) in
	{ addr = from_addr; nodes = nodes }

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
