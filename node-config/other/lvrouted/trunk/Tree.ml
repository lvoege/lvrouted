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

(* Traverse a list of nodes breadth-first, calling a function for every node. The
   callback function is fed three parameters: the gateway address to use, the parent
   node and the node itself. *)
let rec traverse f l = match l with
	  []		-> ()
	| (x,p,gw)::xs	-> f x p gw;
			   traverse f (List.append xs
			   		(List.map (fun node -> node, x, gw) x.nodes))

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
   a list of pruned nodes and a routing table.
   
   1. Initialize a routing table with routes to our own addresses.
   2. Make a temporary node to hook the list in, so the rest of
      the routine can assume to be working on a node structure.
   3. Traverse the tree breadth-first. For every node, check if
      there is a route already. If so, remove this node from
      the list of children in the parent. If not, add a route.
   4. Filter out routes that are included in a route from the
      list of directly attached routes.

   To be able to do this, the callback to the traversal routine
   needs three pieces of information:
     - the node to work on
     - the parent of this node, to be able to remove this node
     - the gateway. The top of every node in the 'nodes'
       parameter is the gateway to the tree under it.

TODO: 4 may be nothing more than cosmetics now that route addition
      finally works right. 4 was added because some of the evidence
      while debugging pointed to such routes acting up. check if it
      is just cosmetic now and note it.
*)
let merge nodes directnets =
	let routes = ref (List.fold_left
				(fun map (a, _) -> IPMap.add a a map)
				IPMap.empty directnets) in
	let nodes = List.map copy nodes in
	let fake = { addr = Unix.inet_addr_any; nodes = nodes } in
	traverse (fun node parent gw ->
			if IPMap.mem node.addr !routes then
			  parent.nodes <- List.filter (fun n ->
			  	n.addr <> node.addr) parent.nodes
			else
			  routes := IPMap.add node.addr gw !routes)
		 (List.map (fun node -> node, fake, node.addr) nodes);
	routes := IPMap.fold (fun a gw map ->
			if List.exists (fun (a', n) ->
				Route.includes_impl a' n a 32) directnets then map
			else IPMap.add a gw map) !routes IPMap.empty;
	fake.nodes, !routes

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
