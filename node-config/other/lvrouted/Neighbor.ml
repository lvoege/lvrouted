(* Neighbor type definition, management and utility functions *)
module Neighbor = struct
open HopInfo
open Common
open LowLevel
open Route

(* A neighbor has a name, an interface and address to reach it on, 
   a timestamp when we last successfully received something from the neighbor,
   and optionally the last hoptable received *)
type t = {
	name: string;
	iface: string;
	addr: Unix.inet_addr;

	mutable last_seen: float;
	mutable hoptable: HopInfo.hopelts option;
}

let show n =
	print_string (n.name ^ ": " ^ Unix.string_of_inet_addr n.addr ^ " on " ^
			n.iface ^ "\n");
	print_newline ()

(* constructor *)
let make name iface addr =
	{ name = name; iface = iface; addr = addr;
	  last_seen = -1.0;
	  hoptable = None }

(* send the given hoptable to the given neighbor under the current routing
   table. hoptable entries going through the neighbor per the routing table
   will be filtered to counter the count-to-infinity problem *)
let send fd routes hs n =
(*
	print_string ("sending hoptable to " ^ n.name);
	print_newline ();
	HopInfo.printelts hs; *)
	let hs' = HopInfo.filter hs routes n.addr in
	try
		HopInfo.send hs' fd n.addr;
	with _ ->
		()

(* Given a list of neighbors, data in a string and the sockaddr it came from,
   handle it. Find the neighbor associated with the address, parse the
   hoptable and mark the time *)
let handle_data ns s sockaddr =
	try	
		let addr = Common.get_addr_from_sockaddr sockaddr in
		let n = List.find (fun n -> n.addr = addr) ns in
		n.hoptable <- Some (HopInfo.from_string s addr);
		n.last_seen <- Unix.time ()
	with _ -> 
		()

(* Given a list of neighbors and interface i, invalidate the hoptables 
   for all the neighbors on that interface *)
let nuke_hoptable_for_iface ns i =
	List.iter (fun n -> if n.iface = i then
			      n.hoptable <- None) ns

(* Given a list of neighbors and a number of seconds, invalidate the 
   hoptables of all neighbors not heard from since numsecs ago *)
let nuke_old_hoptables ns numsecs =
	let limit = (Unix.time ()) -. numsecs in
	let res = ref false in
	List.iter (fun n -> 
		if n.last_seen < limit then begin
		  n.hoptable <- None;
		  res := true
		end) ns;
	!res

(* from the given direct hoptable and list of neighbors, derive a list of
   (unaggregated) routes and a merged hoptable. *)
let derive_routes_and_hoptable direct direct_hash ns = 
	let ns' = List.filter (fun n -> Common.is_some n.hoptable) ns in
	if List.length ns' = 0 then
	  [], [| |]
	else begin
	  let h = Hashtbl.create (Array.length (Common.from_some (List.hd ns').hoptable)) in
	  List.iter (fun n ->
		Array.iter (fun e ->
			let addr = HopInfo.addr e in
			try
				if (not (HopInfo.path_in_hash e direct_hash)) &&
				   HopInfo.compare e (fst (Hashtbl.find h addr)) < 0 then
				  Hashtbl.replace h addr (e, n)
			with Not_found ->
				Hashtbl.add h addr (e, n)
		) (Common.from_some n.hoptable)
	  ) ns';
	  let routes = ref [] in
	  let hoptable = ref [] in
	  Hashtbl.iter (fun a (e, n) ->
		routes := (Route.make a (HopInfo.mask e) n.addr)::(!routes);
		hoptable := e::(!hoptable)
	  ) h;
	  let hoptable' = Array.append direct (Array.of_list (!hoptable)) in
(*
	  print_string "derived table";
	  print_newline ();
	  HopInfo.printelts hoptable'; *)
	  !routes, hoptable'
	end

(* Extract a list of distinct interfaces from the given list of neighbors *)
let extract_ifaces ns =
	let h = Hashtbl.create 4 in
	let ifaces = ref [] in
	List.iter (fun n -> Hashtbl.replace h n.iface 1) ns;
	Hashtbl.iter (fun iface _ -> ifaces := iface::!ifaces) h;
	!ifaces

end (* module Neighbor *)
