(* Interface type definition and management *)
open Common

type t = {
	name: string;
	is_master: bool;

	(* when was the last arpentries and associated update? *)
	mutable last_update: float;
	mutable arpentries: MAC.MacSet.t option;
	mutable associated: MAC.MacSet.t option;
	mutable is_associated: bool option;
}

let make n =
	let iface_is_master name = 
		let c = Unix.open_process_in ("/sbin/ifconfig " ^ name) in
		let re = Str.regexp "^.*media:.*<hostap>" in
		let l = Common.snarf_channel_for_re c re 0 in
		List.length l > 0 in
	{ name = n;
	  is_master = iface_is_master n;
	  last_update = -1.0;
	  arpentries = None;
	  associated = None;
	  is_associated = None }

let associated iface =
	let c = Unix.open_process_in ("/usr/sbin/wicontrol -l -i " ^ iface.name) in
	let re = Str.regexp "^\\([^ ]+\\) .*ASSOC.*" in
	let l = Common.snarf_channel_for_re c re 1 in
	List.fold_left (fun a e -> MAC.MacSet.add e.(1) a) MAC.MacSet.empty l

let update iface =
	let now = Unix.gettimeofday () in
	if (iface.is_master && (Common.is_none iface.associated)) ||
	   (iface.last_update < (now -. 1.0)) then begin
		if iface.is_master then
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
	(in_arptable iface mac) &&
	  ((iface.is_master && (MAC.MacSet.mem mac (Common.from_some iface.associated))) &&
	   (not iface.is_master && (Common.from_some iface.is_associated)))
