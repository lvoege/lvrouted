(* This module defines and implements the tree structure that is passed
   between nodes. It also implements the merging of trees, which is the
   core of the whole routing scheme. *)
open Common

type node = {
	addr: Unix.inet_addr;
	mutable nodes: node list;
}

module IntQueue = PrioQueue.Make(struct
	type t = int
	let compare = compare
end)

(* Constructor *)
let make a nodes = { addr = a; nodes = nodes }

(* Accessors *)
let addr n = n.addr
let nodes n = n.nodes

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
			s := !s ^ i ^ Unix.string_of_inet_addr n.addr ^ "\n";
			show' (indent + 1) n.nodes) l in
	show' 0 l;
	!s

let dump_tree fname nodes =
	let out = open_out (!Common.tmpdir ^ fname) in
	output_string out (show nodes);
	close_out out

let are_two_iphashes_equal one other =
	let l1 = IPHash.fold (fun k v a -> (k, v)::a) one [] in
	let l2 = IPHash.fold (fun k v a -> (k, v)::a) other [] in
	let l1_ = List.sort compare l1 in
	let l2_ = List.sort compare l2 in
	List.fold_left2 (fun a x y -> match a with
		| false -> false
		| true -> compare x y = 0) true l1_ l2_

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
let merge nodes directnets =
	(* step 1 *)
	let routes = IPHash.create 512 in
	List.iter (fun (a, _) -> IPHash.add routes a a) directnets;
	(* step 2 *)
	let fake = make Unix.inet_addr_any [] in
	(* step 3 *)
	let rec traverse pq =
		if pq = IntQueue.empty then ()
		else    let (_, (depth, node, parent, gw), pq') = IntQueue.extract pq in
			if IPHash.mem routes node.addr then
			  traverse pq' (* ignore this node *)
			else begin
				(* copy this node and hook it into the new tree *)
				let newnode = make node.addr [] in
				parent.nodes <- newnode::parent.nodes;
				IPHash.add routes node.addr gw;

				(* Create queue elements for the children of
				   this node and push them on. For now the
				   priority is going to be the depth of the
				   parent plus one, giving normal BFS
				   behavior. *)
				let prio' = depth + 1 in
				let pq'' = List.fold_left (fun pq' child ->
					let child_element = (prio', child, newnode, gw) in
					IntQueue.insert pq' prio' child_element) pq' node.nodes in
				traverse pq''
			end in
	let todo = List.fold_left (fun q node ->
			let e = (0, node, fake, node.addr) in
			IntQueue.insert q 0 e) IntQueue.empty nodes in
	traverse todo;

	(* Verification: do it again using the old algorithm. *)
	let routes_orig = IPHash.create 512 in
	let fake_orig = make Unix.inet_addr_any [] in
	List.iter (fun (a, _) -> IPHash.add routes_orig a a) directnets;
	let rec traverse_orig = function
		  []			-> ()	(* all done *)
		| (node,parent,gw)::xs	-> 
			if IPHash.mem routes_orig node.addr then
			  traverse_orig xs (* ignore this node *)
			else begin
				(* copy this node and hook it into the new tree *)
				let newnode = make node.addr [] in
				parent.nodes <- newnode::parent.nodes;
				IPHash.add routes_orig node.addr gw;
				(* and continue traversing, after appending the children of this node to the todo list *)
				traverse_orig (xs@(List.map (fun node' -> node', newnode, gw) node.nodes))
			end in
	let todo_orig = List.map (fun node -> node, fake_orig, node.addr) nodes in
	traverse_orig todo_orig;
	dump_tree "tree.new" fake.nodes;
	dump_tree "tree.orig" fake_orig.nodes;
	if not (are_two_iphashes_equal routes routes_orig) then begin
		raise (Failure "Eep!");
	end;
	print_string ("YAY!\n");

	(* step 4 *)
	IPHash.iter (fun a gw ->
		if List.exists (fun (a', n) ->
			LowLevel.route_includes_impl a' n a 32) directnets then
		  IPHash.remove routes a) routes;
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
