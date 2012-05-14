(* Interface type definition and management *)
open Common

type ifacetype =
	| WIRED		(* no notion of association *)
	| WIFI_CLIENT	(* client of a master, can check for association *)
	| WIFI_MASTER	(* master of several clients, can check the list of
			   associated stations for a specific client *)

type t = {
	name: string;
	itype: ifacetype;

	(* when was the last arpentries and associated update? *)
	mutable last_assoc_update: float;
	mutable last_arp_update: float;
	mutable arpentries: MAC.Set.t option;
	(* is type is master, this is the set of associated macaddrs*)
	mutable associated: MAC.Set.t option;
	(* is type is client, whether or not this iface is associated
	   with a master *)
	mutable is_associated: bool option;
}

(* Constructor *)
let make n =
	let iface_type name = 
		(* This is pretty lame, but this is executed only once per
		   interface at startup anyway, and it'd be a huge gob of
		   code in lowlevel_c.c, so I'll leave it like this *)
		let c = Unix.open_process_in ("/sbin/ifconfig " ^ name) in
		let re = Str.regexp "^.*media: \\(.*\\)" in
		let l = Common.snarf_channel_for_re c re 2 in
		ignore(Unix.close_process_in c);
		if l = [] then WIRED else
			let media = (List.hd l).(1) in
			if LowLevel.strstr media "hostap" <> -1 then
			  WIFI_MASTER
			else if LowLevel.strstr media "Wireless" <> -1 then
			  WIFI_CLIENT
			else
			  WIRED in
	{ name = n;
	  itype = iface_type n;
	  last_assoc_update = -1.0;
	  last_arp_update = -1.0;
	  arpentries = None;
	  associated = None;
	  is_associated = None }

let itype i = i.itype

(* Return a MAC.Set of addresses that are associated with the given interface *)
let associated ifname =
	Array.fold_left (fun a e -> MAC.Set.add e a)
			MAC.Set.empty
			(LowLevel.get_associated_stations ifname)

(* Update the information about associations on the given interface, if it's
   been long enough since the last update. *)
let update iface =
	let now = Unix.gettimeofday () in
	if (iface.itype = WIFI_MASTER && (Common.is_none iface.associated)) ||
	   (iface.last_assoc_update < (now -. Common.iface_assoc_update)) then begin
		if iface.itype = WIFI_MASTER then begin
			let a = associated iface.name in
			iface.associated <- Some a;
			Log.lazylog Log.debug (fun _ -> 
				["Associated stations:"]@
				List.map MAC.ether_ntoa (MAC.Set.elements a));
		end else if iface.itype = WIFI_CLIENT then
		  iface.is_associated <- Some (LowLevel.iface_is_associated iface.name);
		iface.last_assoc_update <- now
	end;
	if iface.last_arp_update < (now -. Common.iface_arp_update) then begin
		let arptable = MAC.get_arptable iface.name in
		(* Fold the output of the lowlevel MAC.arptable into a set of
		   mac addresses, which is all that's needed here. *)
		let arpset = IPMap.fold (fun _ -> MAC.Set.add)
					arptable MAC.Set.empty in
		iface.arpentries <- Some arpset;
		iface.last_arp_update <- now;
		Log.lazylog Log.debug (fun _ -> [MAC.show_arptable arptable]);
	end

(* Is the given MAC address in the given interface's arp table? *)
let in_arptable iface mac =
	update iface;
	MAC.Set.mem mac (Common.from_some iface.arpentries)

(* Is the given mac address reachable over the given interface? *)
let is_reachable iface mac =
	update iface;
	in_arptable iface mac && match iface.itype with
	    WIRED -> true
	  | WIFI_MASTER -> MAC.Set.mem mac (Common.from_some iface.associated)
	  | WIFI_CLIENT -> Common.from_some iface.is_associated

let is_nanostation iface =
	MAC.Set.exists (fun a ->
		let s = MAC.ether_ntoa a in
		let s' = String.sub s 0 8 in
		(String.compare s' "00:15:6d") == 0) (Common.from_some iface.arpentries)
