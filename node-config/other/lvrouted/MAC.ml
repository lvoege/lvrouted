(* MAC address handling *)
open Common

(* A mac address is just a string, a string being the way to pass around
   binary data as well as printable data in ocaml. This string is always
   of length ETHER_ADDR_LEN (== 6) and is binary data. *)
type t = string

type arptable = (Unix.inet_addr, string) Hashtbl.t

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
let arptable iface : arptable = 
	let now = Unix.gettimeofday () in
	if !arptables_last_update < now -. Common.arptables_update_every then begin
		Log.log Log.debug "getting arp entries";
		let a = LowLevel.get_arp_entries () in
		Array.iter (fun (iface, ipaddr, macaddr) ->
			let h = try StringMap.find iface !arptables
				with Not_found ->
					let h' = Hashtbl.create 8 in
					arptables := StringMap.add iface h' !arptables;
					h' in
			Hashtbl.add h ipaddr macaddr) a;
		arptables_last_update := now
	end;
	try StringMap.find iface !arptables
	with Not_found -> Hashtbl.create 1

let show_arptable h =
	Hashtbl.fold (fun addr mac a ->
		a ^ "\t" ^
		Unix.string_of_inet_addr addr ^ " -> " ^
		ether_ntoa mac ^ "\n"
	) h "Arptable: \n"
