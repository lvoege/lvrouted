(* MAC address handling *)
type t = string

type arptable = (Unix.inet_addr, string) Hashtbl.t

module MacSet = Set.Make(String)

(* Keep one global hash from interface name to interface-specific arp table,
   together with some information to be able to refresh it every now and
   then.  *)
let arptables = Hashtbl.create 8
let arptables_last_update = ref (-1.0)
let arptables_update_every = 60.0

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
	if !arptables_last_update < now -. arptables_update_every then begin
		let c = Unix.open_process_in ("/usr/sbin/arp -a -n") in
		let re = Str.regexp "^[^ ]+ (\\([^)]+\\)) at \\([0-9a-f:]+\\) on \\([^ ]+\\) .*" in
		let l = Common.snarf_channel_for_re c re 4 in
		let h = Hashtbl.create (List.length l) in
		List.iter (fun a ->
			let h = try Hashtbl.find arptables a.(3)
				with Not_found ->
					let h' = Hashtbl.create 8 in
					Hashtbl.add arptables a.(3) h';
					h' in
			Hashtbl.add h (Unix.inet_addr_of_string a.(1))
				      (ether_aton a.(2))) l;
		ignore(Unix.close_process_in c);
		arptables_last_update := now
	end;
	try Hashtbl.find arptables iface
	with Not_found -> Hashtbl.create 1

let show_arptable (h: arptable) =
	Hashtbl.fold (fun addr mac a ->
		a ^ "\t" ^
		(Unix.string_of_inet_addr addr) ^ " -> " ^
		(ether_ntoa mac) ^ "\n"
	) h "Arptable: \n"
