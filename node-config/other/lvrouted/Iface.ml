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
	mutable last_update: float;
	mutable arpentries: MAC.MacSet.t option;
	mutable associated: MAC.MacSet.t option;
	mutable is_associated: bool option;
}

let make n =
	let iface_type name = 
		let c = Unix.open_process_in ("/sbin/ifconfig " ^ name) in
		let re = Str.regexp "^.*media: \\(.*\\)" in
		let l = Common.snarf_channel_for_re c re 2 in
		ignore(Unix.close_process_in c);
		let media = (List.hd l).(1) in
		if LowLevel.strstr media "hostap" != -1 then
		  WIFI_MASTER
		else if LowLevel.strstr media "Wireless" != -1 then
		  WIFI_CLIENT
		else
		  WIRED in
	{ name = n;
	  itype = iface_type n;
	  last_update = -1.0;
	  arpentries = None;
	  associated = None;
	  is_associated = None }

let associated iface =
	let c = Unix.open_process_in ("/usr/sbin/wicontrol -i " ^ iface.name ^ " -l") in
	let re = Str.regexp "^\\([^ ]+\\) .*ASSOC.*" in
	let l = Common.snarf_channel_for_re c re 2 in
	ignore(Unix.close_process_in c);
	List.fold_left (fun a e -> MAC.MacSet.add (MAC.ether_aton e.(1)) a) MAC.MacSet.empty l

let update iface =
	let now = Unix.gettimeofday () in
	if (iface.itype = WIFI_MASTER && (Common.is_none iface.associated)) ||
	   (iface.last_update < (now -. 1.0)) then begin
		if iface.itype = WIFI_MASTER then
		  iface.associated <- Some (associated iface)
		else
		  iface.is_associated <- Some (LowLevel.iface_is_associated iface.name);
		iface.last_update <- now
	end

let in_arptable iface mac =
	if Common.is_none iface.arpentries then
	  iface.arpentries <- Some
	  	(Hashtbl.fold (fun a m s ->
				MAC.MacSet.add m s)
			      (MAC.arptable iface.name) MAC.MacSet.empty);
	MAC.MacSet.mem mac (Common.from_some iface.arpentries)

let is_reachable iface mac =
	update iface;
	if in_arptable iface mac then begin
	  (iface.itype = WIRED ||
	   (iface.itype = WIFI_MASTER && (MAC.MacSet.mem mac (Common.from_some iface.associated))) ||
	   (iface.itype = WIFI_CLIENT && (Common.from_some iface.is_associated)))
	end else begin
		false;
	end
