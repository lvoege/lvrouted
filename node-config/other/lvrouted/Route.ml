(* Route type definition and management *)
type route = {
	addr: Unix.inet_addr;
	mask: int;
	gw: Unix.inet_addr;
}

(* Make a Set of routes *)
module RouteType = struct
	type t = route
	(* compare first on the netmask, then on the address and finally on the gateway *)
	let compare a b =
		let res = compare a.mask b.mask in
		if res = 0 then
		  let res' = compare a.addr b.addr in
		  if res' = 0 then
		    compare a.gw b.gw
		  else
		    res'
		else
		  res
end
module Set = Set.Make(RouteType)
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

(* Given a list of routes, find the gateway for the given addr. *)
let lookup routes addr =
	let matches = Set.filter (fun r -> matches r addr) routes in
	if Set.is_empty matches then
	  raise Not_found
	else begin
		let es = List.sort (fun a b -> compare a.mask b.mask) (Set.elements matches) in
		(List.hd es).gw
	end

let show r =
	Unix.string_of_inet_addr r.addr ^ "/" ^ string_of_int r.mask ^ " -> " ^
	Unix.string_of_inet_addr r.gw

let showroutes rs = 
	let l = List.fast_sort (fun a b -> compare a.addr b.addr)
			       (Set.elements rs) in
	List.fold_left (fun a r -> a ^ "\t" ^ show r ^ "\n")
		       "Route table:\n" l

(* Given a list of routes, try to clump together as many routes as possible.

   Take the first route on the todo list:

     If the netmask is 0, return the route as the only route. It'll be the
     default route.
    
     Else expand the netmask by one bit. Check if it gobbles up any routes
     to different gateways.
       If so, move the unexpanded route to the done list and recurse
       If not, remove all routes now covered by the newly expanded route from
         the todo list and recurse. *)
let aggregate routes =
	let rec aggregate' todo done_ =
		match todo with
		  []		-> done_
		| r :: rs	->
			if r.mask = 0 then
			  [ r ]
			else if r.addr = r.gw && r.mask = 32 then
			  aggregate' rs done_
			else begin
			  let r' = { r with mask = r.mask - 1 } in
			  let f t = t.gw != r.gw && includes r' t in
			    if List.exists f (rs@done_) then
				  aggregate' rs (r::done_)
			    else let rs' = List.filter (fun t ->
						not (includes r' t)) rs in
				  aggregate' (r'::rs') done_
			end in
	List.fold_left (fun a r -> Set.add r a) Set.empty (aggregate' (Set.elements routes) [])

(* Given a set of old routes and a set of new routes, produce a list
   of routes to delete and another list of routes to add *)
let diff oldroutes newroutes =
	Set.elements (Set.diff oldroutes newroutes),
	Set.elements (Set.diff newroutes oldroutes)

(* Don't use, call commit instead. TODO: move to LowLevel.ml. This is
   not trivial because of a then cyclic include, which would need to
   be broken. *)
external commit: route list -> route list ->
		 ( (route * string) list * (route * string) list )
			
  = "routes_commit"

external fetch: unit -> route list
  = "routes_fetch"

let flush () =
	(* TODO, figure out why this doesn't work:
	commit (fetch ()) []

	I suspect the route deletes need all the sockaddrs that were
	specified for a route to be able to delete it, and this only
	propagates dst, gw and netmask.  *)
	let c = Unix.open_process_in "/sbin/route -q flush" in
	ignore(Unix.close_process_in c);
