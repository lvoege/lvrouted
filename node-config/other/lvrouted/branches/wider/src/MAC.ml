(* MAC address handling *)
open Common

(* A mac address is just a string, a string being the way to pass around
   binary data as well as printable data in ocaml. This string is always
   of length ETHER_ADDR_LEN (== 6) and is binary data. *)
type t = string

type arptable = string IPMap.t

module Set = Set.Make(String)

(* Keep one global map from interface name to interface-specific arp table,
   together with some information to be able to refresh it every now and
   then.  *)
let arptables = ref StringMap.empty
let arptables_last_update = ref (-1.0)

let ether_aton s =
	let s' = String.create 6 in
	if not (LowLevel.ether_aton s s') then
	  raise (Failure "Cannot parse MAC address");
	s'

let ether_ntoa s =
	let s' = String.create 17 in
	if not (LowLevel.ether_ntoa s s') then
	  raise (Failure "Invalid MAC address");
	s'

(* Return the arptable for the given interface. If it's time to update the
   global arptable hash, do so. Dereference the arp hash. *)
let get_arptable iface : arptable = 
	let now = Unix.gettimeofday () in
	if !arptables_last_update < now -. Common.arptables_update_every then begin
		Log.log Log.debug "getting arp entries";
		let a = LowLevel.get_arp_entries () in
		arptables := List.fold_left (fun map (iface, ip, mac) ->
			let ifmap = try  StringMap.find iface map
				    with Not_found -> IPMap.empty in
			let ifmap = IPMap.add (ip, 32) mac ifmap in
			StringMap.add iface ifmap map) StringMap.empty a;
		arptables_last_update := now
	end;
	try StringMap.find iface !arptables
	with Not_found -> IPMap.empty

let show_arptable h =
	IPMap.fold (fun (addr, _) mac a ->
		a ^ "\t" ^
		Unix.string_of_inet_addr addr ^ " -> " ^
		ether_ntoa mac ^ "\n") h "Arptable: \n"
