(* Route type definition and management *)
module Route = struct

type t = {
	addr: Unix.inet_addr;
	mask: int;
	gw: Unix.inet_addr;
}

type routes = t list

(* constructor *)
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
	let route = List.find (fun r -> matches r addr) routes in
	route.gw

let show r =
	Unix.string_of_inet_addr r.addr ^ "/" ^ string_of_int r.mask ^ " -> " ^
	Unix.string_of_inet_addr r.gw

let rec aggregate todo done_ =
	match todo with
	  []		-> done_
	| r :: rs	->
		if r.mask = 0 then
		  [ r ]
		else begin
		  let r' = { r with mask = r.mask - 1 } in
		  let f t = t.gw != r.gw && includes r' t in
		    if List.exists f (rs@done_) then
		  	  aggregate rs (r::done_)
		    else let rs' = List.filter (fun t ->
		    			not (includes r' t)) rs in
		  	  aggregate (r'::rs') done_
		end

end (* module Route *)
