(* MAC address handling *)
type t = string

type arptable = (Unix.inet_addr, string) Hashtbl.t

module MacSet = Set.Make(String)

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

let arptable iface : arptable = 
	let c = Unix.open_process_in ("/usr/sbin/arp -a -i " ^ iface) in
	let re = Str.regexp "^[^ ]+ (\\([^)]+\\)) at \\([^ ]+\\) on \\([^ ]+\\) .*" in
	let l = Common.snarf_channel_for_re c re 3 in
	let h = Hashtbl.create (List.length l) in
	List.iter (fun a ->
		Hashtbl.add h (Unix.inet_addr_of_string a.(1))
			      (ether_aton a.(2))) l;
	h

let show_arptable (h: arptable) =
	Hashtbl.fold (fun addr mac a ->
		a ^ "\t" ^
		(Unix.string_of_inet_addr addr) ^ " -> " ^
		(ether_ntoa mac) ^ "\n"
	) h "Arptable: \n"
