(* Route type definition and management *)

type route = {
	addr: Unix.inet_addr;
	mask: int;
	gw: Unix.inet_addr;
}

(* Make a Set of routes. Consider two routes equal only when both their
   address and netmask are equal. The gateway can differ. This makes the
   set operations later on easier. The aggregator code does take
   different gateways into account. *)
module Set = Set.Make(struct
	type t = route
	(* compare first on the address and then on the netmask *)
	let compare a b = 
		let r = compare a.addr b.addr in
		if r = 0 then
		  compare a.mask b.mask
		else r
end)
(* Make a Map with an address as its key *)
module Map = Map.Make(struct
	type t = Unix.inet_addr
	let compare = compare
end)

(* Constructor *)
let make a m g = { addr = a; mask = m; gw = g }

let includes_impl a m1 b m2 =
	(m1 <= m2) &&
	  (LowLevel.mask_addr a m1 =
	   LowLevel.mask_addr b m1)

(* Does route a completely include b? *)
let includes a b =
	includes_impl a.addr a.mask b.addr b.mask

(* Does the given addr fall in the given route *)
let matches route addr =
	LowLevel.mask_addr route.addr route.mask =
	LowLevel.mask_addr addr route.mask

let show r =
	Unix.string_of_inet_addr r.addr ^ "/" ^ string_of_int r.mask ^ " -> " ^
	Unix.string_of_inet_addr r.gw

(* turn a list of routes into a set of routes *)
let make_set = List.fold_left (fun a r -> Set.add r a) Set.empty

let showroutes rs = 
	List.fold_left (fun a r -> a ^ "\t" ^ show r ^ "\n")
		       "Route table:\n" (Set.elements rs)

(* Given a list of routes, try to clump together as many routes as possible.

   Take the first route on the todo list:

     If the netmask is the minimum netmask, move the route to the done list
     and recurse.
    
     Else expand the netmask by one bit. Check if it gobbles up any routes
     to different gateways.
       If so, move the unexpanded route to the done list and recurse.
       If not, remove all routes now covered by the newly expanded route from
         the todo list and recurse.

   Finally, take the now aggregated list of routes and create a set of routes,
   with the addresses of the routes masked according to their netmask.
*)
let aggregate routes =
	let rec aggregate' todo done_ =
		match todo with
		  []		-> done_
		| r :: rs	->
			if LowLevel.addr_is_ipv6 r.addr ||
			   r.mask = !Common.min_mask then
			  aggregate' rs (r::done_)
			else if r.addr = r.gw && r.mask = 32 then
			  aggregate' rs done_
			else begin
			  let r' = { r with mask = r.mask - 1 } in
			  let f t = t.gw <> r.gw && includes r' t in
			  if List.exists f (rs@done_) then
				aggregate' rs (r::done_)
			  else let rs' = List.filter (fun t ->
						not (includes r' t)) rs in
				   aggregate' (r'::rs') done_
			end in
	List.fold_left (fun set r ->
			Set.add { r with addr = LowLevel.mask_addr r.addr r.mask} set)
		       Set.empty (aggregate' (Set.elements routes) [])

(* Given a set of old routes and a set of new routes, produce a list
   of routes to delete, a list of routes to add and a list of routes
   that changed their gateway.

   Deletes and adds are easy using set operations. Changes are less
   easy:
     - Build a map from address to route for both the old and the new routes
     - Intersect the old and the new routes. The set type orders on address
       and netmask, so the intersection has all routes for which neither
       address or netmask have changed. These should now be checked to see
       if they've changed gateways.
     - Fold this intersection, building the set of routes that changed
       gateways along the way. For the given route, look up old and new and
       compare the gateway. If different, add to the set, else pass along
       the set unaltered. *)
let diff oldroutes newroutes =
	let dels = Set.diff oldroutes newroutes in
	let adds = Set.diff newroutes oldroutes in

	let oldmap = Set.fold (fun r -> Map.add r.addr r)
		     oldroutes Map.empty in
	let newmap = Set.fold (fun r -> Map.add r.addr r)
		     newroutes Map.empty in
	let isect = Set.inter oldroutes newroutes in
	let changes = Set.fold (fun r set ->
			let old_r = Map.find r.addr oldmap in
			let new_r = Map.find r.addr newmap in
			if old_r.gw <> new_r.gw then
			  Set.add new_r set
			else
			  set) isect Set.empty in
	dels, adds, changes

(* Commit the given list of adds, deletes and changes to the kernel.
   Don't use directly, use commit instead.

   TODO: move to LowLevel.ml. This is not trivial because of a then
   cyclic include, which would need to be broken. *)
external lowlevel_commit:
		 Unix.file_descr		(* sockfd *)
	      -> route list			(* deletes *)
	      -> route list			(* adds *)
	      -> route list			(* changes *)
	      -> ( (route * string) list	(* delete errors *)
	      	 * (route * string) list	(* add errors *)
	      	 * (route * string) list)	(* change errors *)
  = "routes_commit"

(* Return a list of all routes with a gateway in the kernel route table. *)
external lowlevel_fetch: unit -> route list
  = "routes_fetch"

(* Return a list of all routes to routable addresses and with a gateway in
   the kernel route table. *)
let fetch () =
	let rs = lowlevel_fetch () in
	make_set (List.filter (fun r -> Common.addr_in_range r.addr) rs)

(* Commit the given list of adds, deletes and changes to the kernel.
   Attempt a maximum of five extra iterations of checking whether or
   not every change was applied, and redoing those that weren't. *)
let commit fd dels adds chgs = 
	let res = lowlevel_commit fd
				  (Set.elements dels)
				  (Set.elements adds)
				  (Set.elements chgs) in
	let a = ref adds in	(* Still to add *)
	let d = ref dels in	(* Still to delete *)
	let _ = Common.try_max_times 5 (fun i ->
		Log.log Log.debug ("iteration " ^ string_of_int i);
		Unix.sleep 1;
		let rs = fetch () in
		d := Set.inter !d rs;
		Log.lazylog Log.debug (fun _ -> ["Still to delete:"; showroutes !d]);
		a := Set.diff !a rs;
		Log.lazylog Log.debug (fun _ -> ["Still to add:"; showroutes !a]);
		ignore(lowlevel_commit fd
				       (Set.elements !d)
				       (Set.elements !a) []);
		Set.cardinal !d = 0 &&
		Set.cardinal !a = 0) in
	Log.lazylog Log.debug (fun _ -> ["Current routing table:"; showroutes (fetch ())]);
	res

(* Try to have the kernel get rid of all gateway routes *)
let flush fd =
	Log.log Log.debug "flushing routes";
	let logerr (r, s) = Log.log Log.debug (show r ^ ": " ^ s) in
	Common.try_max_times Common.max_route_flush_tries (fun _ ->
		let rs = fetch () in
		Log.log Log.debug ("I have " ^ string_of_int (Set.cardinal rs)
				 ^ " routes to delete");
		let delerrs, _, _ = lowlevel_commit fd (Set.elements rs) [] [] in
		List.iter logerr delerrs;
		Set.cardinal rs = 0);
