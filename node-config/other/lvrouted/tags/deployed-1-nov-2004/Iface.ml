(* Interface type definition and management *)
open Common

type ifacetype =
	| WIRED
	| WIFI_CLIENT
	| WIFI_MASTER

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
		let c = Unix.open_process_in ("/sbin/ifconfig " ^ name) in
		let re = Str.regexp "^.*media: \\(.*\\)" in
		let l = Common.snarf_channel_for_re c re 2 in
		ignore(Unix.close_process_in c);
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
		if iface.itype = WIFI_MASTER then
		  iface.associated <- Some (associated iface.name)
		else if iface.itype = WIFI_CLIENT then
		  iface.is_associated <- Some (LowLevel.iface_is_associated iface.name);
		iface.last_assoc_update <- now
	end;
	if iface.last_arp_update < (now -. Common.iface_arp_update) then begin
		iface.arpentries <- Some
			(Hashtbl.fold (fun a m s -> MAC.Set.add m s)
				      (MAC.arptable iface.name) MAC.Set.empty);
		iface.last_arp_update <- now
	end

(* Is the given MAC address in the given interface's arp table? *)
let in_arptable iface mac =
	(* If the interface's table isn't there (hasn't been read, or expired)
	   try and read it. This shouldn't happen, but check just in case
	   we get called from another module that doesn't call update prior to
	   this *)
	if Common.is_none iface.arpentries then
	  iface.arpentries <- Some
	  	(Hashtbl.fold (fun a m s -> MAC.Set.add m s)
			      (MAC.arptable iface.name) MAC.Set.empty);
	MAC.Set.mem mac (Common.from_some iface.arpentries)

(* Is the given mac address reachable over the given interface? *)
let is_reachable iface mac =
	update iface;
	(in_arptable iface mac) &&
	  (iface.itype = WIRED ||
	   (iface.itype = WIFI_MASTER && (MAC.Set.mem mac (Common.from_some iface.associated))) ||
	   (iface.itype = WIFI_CLIENT && (Common.from_some iface.is_associated)))
