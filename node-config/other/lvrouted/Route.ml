(* Route type definition and management *)
type route = {
	addr: Unix.inet_addr;
	mask: int;
	gw: Unix.inet_addr;
}

(* Make struct with an OrderedType signature, to build a Set later on. *)
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

(* And make a Set of routes *)
module RouteSet = Set.Make(RouteType)

(* Constructor *)
let make a m g = { addr = a; mask = m; gw = g }

(* Does route a completely include b? *)
let includes a b =
	(a.mask <= b.mask) &&
	  (LowLevel.mask_addr a.addr a.mask =
	   LowLevel.mask_addr b.addr a.mask)

(* Does the given addr fall in the given route *)
let matches route addr =
	LowLevel.mask_addr route.addr route.mask =
	LowLevel.mask_addr addr route.mask

(* Given a list of routes, find the gateway for the given addr. *)
let lookup routes addr =
	let matches = RouteSet.filter (fun r -> matches r addr) routes in
	if RouteSet.is_empty matches then
	  raise Not_found
	else begin
		let es = List.sort (fun a b -> compare a.mask b.mask) (RouteSet.elements matches) in
		(List.hd es).gw
	end

let show r =
	Unix.string_of_inet_addr r.addr ^ "/" ^ string_of_int r.mask ^ " -> " ^
	Unix.string_of_inet_addr r.gw

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
			else begin
			  let r' = { r with mask = r.mask - 1 } in
			  let f t = t.gw != r.gw && includes r' t in
			    if List.exists f (rs@done_) then
				  aggregate' rs (r::done_)
			    else let rs' = List.filter (fun t ->
						not (includes r' t)) rs in
				  aggregate' (r'::rs') done_
			end in
	List.fold_left (fun a r -> RouteSet.add r a) RouteSet.empty (aggregate' (RouteSet.elements routes) [])

(* Given a set of old routes and a set of new routes, produce a list
   of routes to delete and another list of routes to add *)
let diff oldroutes newroutes =
	RouteSet.elements (RouteSet.diff oldroutes newroutes),
	RouteSet.elements (RouteSet.diff newroutes oldroutes)

(* Don't use, call commit instead *)
external routes_commit: route array -> int -> route array -> int -> int
  = "routes_commit"

let commit deletes adds =
	ignore(routes_commit (Array.of_list deletes) (List.length deletes)
			     (Array.of_list adds) (List.length adds))
