(* Main module. Start reading at the last function *)
open Common
open Log
open Neighbor

(* First some globals. These are global because the signal handlers need to be
   able to set them when re-reading the node's configuration. *)

(* A set of all neighbors *)
let neighbors = ref Neighbor.Set.empty
(* Subset of neighbors that are across wireless links *)
let neighbors_wireless = ref Neighbor.Set.empty
(* Subset of neighbors that are across wired links *)
let neighbors_wired = ref Neighbor.Set.empty
(* The set of IP addresses of wired neighbors *)
let neighbors_wired_ip = ref IPSet.empty
let neighbors_wireless_ip = ref IPSet.empty
(* A dictionary mapping from interface name ('ep0', 'sis1', etc) to Iface.t *)
let ifaces = ref StringMap.empty
(* A list of Tree.nodes's for every one of 'our' addresses. *)
let direct : Tree.node list ref = ref []
(* A list of address, netmask tuples of the same *)
let directnets : (Unix.inet_addr * int) list ref = ref []
(* last broadcast timestamp *)
let last_time = ref 0.0
(* An entry means that neighbor was unreachable last iteration *)
let unreachable = ref Neighbor.Set.empty
(* Resume from saved state on startup instead of a new, clean state *)
let resume = ref false

(* See if any previously reachable neighbors became reachable or vice-versa *)
let check_reachable _ =
	(* See what neighbors are unreachable. Per neighbor, fetch the
	   corresponding Iface.t and see if it's reachable. *)
	let new_unreachable = Neighbor.Set.filter (fun n ->
		Log.log Log.debug ("looking at " ^ Neighbor.show n);
		let iface = StringMap.find (Neighbor.iface n) !ifaces in
		not (Neighbor.check_reachable n iface)) !neighbors in

	let newly_unreachable = Neighbor.Set.diff new_unreachable !unreachable in
	let newly_reachable = Neighbor.Set.diff !unreachable new_unreachable in
	let reachable_changed = not (Neighbor.Set.is_empty newly_unreachable)
			     || not (Neighbor.Set.is_empty newly_reachable) in
	(* Log any changes *)
	if reachable_changed then begin
		let log s n = Log.log Log.info (Neighbor.show n ^ " became " ^ s) in
		Neighbor.Set.iter (log "unreachable") newly_unreachable;
		Neighbor.Set.iter (log "reachable") newly_reachable;
	end;
	(* And update the global *)
	unreachable := new_unreachable;
	reachable_changed

(* This function is the main work horse. It:

     - merges received spanning trees into a new spanning tree.
     - derives a new routing table from the merged spanning tree
     - applies the changes between the old and the new routing table to
       the kernel (if configured to do so)
     - sends the new spanning tree to the neighbors
 *)
let broadcast_run udpsockfd rtsockfd =
	  Log.log Log.debug "starting broadcast run";
	  last_time := Unix.gettimeofday ();

	  (* DEBUG: Dump the incoming trees to the filesystem *)
	  Neighbor.Set.iter (fun n ->
	  	let nname = Neighbor.name n in
	  	let fname = !Common.tmpdir ^ "lvrouted.tree-" ^ nname in
	  	if Common.is_some n.tree then begin
			let out = open_out (!Common.tmpdir ^ "lvrouted.tree-" ^ nname) in
			output_string out (Tree.show [Common.from_some n.tree]);
			close_out out
		end else if Sys.file_exists fname then Sys.remove fname) !neighbors;

	  let newroutes, nodes =
		Neighbor.derive_routes_and_mytree !directnets
						  !neighbors in
	  let nodes = List.append nodes !direct in 

	  (* DEBUG: dump the derived tree to the filesystem *)
	  Tree.dump_tree "lvrouted.mytree" nodes;

	  Neighbor.bcast udpsockfd nodes !neighbors;

	  if !Common.real_route_updates then begin
		let deletes, adds, changes = Route.diff (Route.fetch ()) newroutes in

		(* log the updates *)
		let log_set t s =
			if Route.Set.is_empty s then [] 
			else t::List.map Route.show
					   (Route.Set.elements s) in
		Log.lazylog Log.info (fun _ ->
			log_set "Deletes:" deletes @
			log_set "Adds:" adds @
			log_set "Changes:" changes);

		(* commit the updates to the kernel *)
		try
			let delerrs, adderrs, changeerrs =
				Route.commit rtsockfd deletes adds changes in
			
			Log.lazylog Log.info (fun _ ->
				List.map (fun (r, s) -> Route.show r ^ " got error " ^ s)
					 (delerrs @ adderrs @ changeerrs));
		with Failure s ->
			Log.log Log.errors ("Couldn't update routing table: " ^ s)
		   | _ ->
			Log.log Log.errors ("Unknown exception updating routing table")
	  end else begin
		let out = open_out (!Common.tmpdir ^ "lvrouted.routes") in
		output_string out (Route.showroutes newroutes);
		close_out out;
	  end;
	  Log.log Log.debug "finished broadcast run"

(* This function is called periodically from the select() loop. It decides
   whether or not to start a broadcast_run by checking for changes in
   reachability, expired trees or if it's just time to do so. *)
let periodic_check udpsockfd rtsockfd =
	Log.log Log.debug "in alarm_handler";

	let expired = Neighbor.nuke_old_trees !neighbors Common.timeout in

	let now = Unix.gettimeofday () in
	let its_time = (now -. !last_time) > !bcast_interval in

	(* If there's enough reason, derive a new routing table and a new
	   tree and send it around. *)
	if check_reachable () || expired || its_time then
	  broadcast_run udpsockfd rtsockfd

let abort_handler _ =
	Log.log Log.info "Exiting.";
	exit 0

let read_config _ =
	(* Get all routable addresses that also have a netmask. *)
	let routableaddrs = List.fold_left (fun l (iface, _, a, n, _, _) ->
		if Common.addr_in_range a && Common.is_some n then
		  (iface, a, LowLevel.bits_in_inet_addr (Common.from_some n))::l
		else
		  l) [] (LowLevel.getifaddrs ()) in
	
	(* Construct the direct and directnets lists. direct is a list of
	   Tree.node's for every directly attached address. directnets is
	   a list of (address, masklength) tuples for every directly
	   attached address.*)
	direct := List.map (fun (_, a, _) -> Tree.make a []) routableaddrs;
	directnets := List.map (fun (_, a, n) -> a, n) routableaddrs;

	if !configfile <> "" then begin try
		let chan = open_in !configfile in
		let lines = snarf_lines_from_channel chan in
		close_in chan;
		let extraaddrs = List.map Unix.inet_addr_of_string lines in
		direct := !direct@(List.map (fun a -> Tree.make a []) extraaddrs);
		directnets := !directnets@(List.map (fun a -> a, 32) extraaddrs);
	with _ ->
		Log.log Log.warnings ("Couldn't read the specified config file");
	end;

	(* Interlinks can be recognised by routable addresses and a netmask
	   geq Common.interlink_netmask. *)
	let interlinks = List.filter (fun (_, _, n) -> n >= Common.interlink_netmask && n < 31) routableaddrs in

	(* From the eligible interlinks, create a list of (interface name,
	   address) tuples for all the usable addresses other than our own in
	   the interlink blocks. *)
	let neighboraddrs = List.concat (
		List.map (fun (iface, a, n) ->
			let addrs = LowLevel.get_addrs_in_block a n in
			let addrs' = List.filter (fun a' -> a' <> a) addrs in
			List.map (fun a -> iface, a) addrs') interlinks) in

	(* Construct the global ifaces map and neighbors sets *)
	let ifaces', neighbors' = List.fold_left (
		fun (ifacemap, neighbors) (iface, a) ->
			let name = Unix.string_of_inet_addr a in
			Log.log Log.debug ("neighbor " ^ name ^ " on " ^ iface);
			let i = Iface.make iface in
			let n = Neighbor.make iface a in
			StringMap.add iface i ifacemap,
			Neighbor.Set.add n neighbors)
		(StringMap.empty, Neighbor.Set.empty) neighboraddrs in
	ifaces := ifaces';
	neighbors := neighbors';

	(* Split the set of neighbors into those neighbors attached over a
	   wireless link and those attached over a wired link. For the wired
	   neighbors, make a set of ip addresses to pass to
	   Tree.promote_wired_children *)
	let p, q = Neighbor.Set.partition (fun n ->
			let i = StringMap.find n.iface ifaces' in
			Iface.itype i = Iface.WIRED) neighbors' in
	neighbors_wired := p;
	neighbors_wireless := q;
	neighbors_wired_ip := Neighbor.Set.fold (fun n -> IPSet.add n.addr)
						!neighbors_wired IPSet.empty;
	neighbors_wireless_ip := Neighbor.Set.fold (fun n -> IPSet.add n.addr)
						!neighbors_wireless IPSet.empty

let handle_routemsg udpsockfd rtsockfd = function
	  LowLevel.RTM_NEWADDR (iface, addr, mask) ->
	  	if Common.addr_in_range addr then begin
			read_config ();
			broadcast_run udpsockfd rtsockfd
		end
	| LowLevel.RTM_DELADDR (iface, addr, mask) ->
		if Common.addr_in_range addr then begin
			read_config ();
			broadcast_run udpsockfd rtsockfd
		end
	| _ -> ()
	
let version_info =
		["Version info: ";
		 "svn rev: " ^ string_of_int Version.version;
		 "branch: " ^ Version.branch;
		 "compile host: " ^ Version.host;
		 "compile date: " ^ Version.date;
		 "compiled by: " ^ Version.who;
		 "ocamlopt: " ^ Version.ocamlopt; ]

let print_version _ =
	List.iter (fun s -> print_string s; print_newline ()) version_info;
	exit 1

let dump_version _ =
	let out = open_out (!Common.tmpdir ^ "lvrouted.version") in
	List.iter (fun s -> output_string out (s ^ "\n")) version_info;
	close_out out

(* This, and the converse read_state, aid in the debugging of problems on live
   installations on cramped nodes. kill -USR2 the daemon and all state will
   be dumped to lvrouted.state. copy to the development machine and read_state
   it and go from there. *)
let dump_state _ =
	let state = !neighbors, !neighbors_wireless, !neighbors_wired,
		!neighbors_wired_ip, !neighbors_wireless_ip,
		!ifaces, !direct, !directnets,
		!unreachable, !MAC.arptables in
	let out = open_out (!Common.tmpdir ^ "lvrouted.state") in
	output_string out (Marshal.to_string state []);
	close_out out

let read_state s =
	let neighbors', neighbors_wireless', neighbors_wired',
		neighbors_wired_ip', neighbors_wireless_ip',
		ifaces', direct', directnets', unreachable',
		arptables' = Marshal.from_string s 0 in
	neighbors := neighbors';
	neighbors_wireless := neighbors_wireless';
	neighbors_wired := neighbors_wired';
	neighbors_wired_ip := neighbors_wired_ip';
	neighbors_wireless_ip := neighbors_wireless_ip';
	ifaces := ifaces';
	direct := direct';
	directnets := directnets';
	unreachable := unreachable';
	MAC.arptables := arptables'

let argopts = [
	"-a", Arg.Set_float Common.alarm_timeout, "Interval between checking for interesting things";
	"-b", Arg.Set_float Common.bcast_interval, "Interval between contacting neighbors";
	"-c", Arg.Set_string configfile, "Config file";
	"-d", Arg.Set_int Log.loglevel, "Loglevel. Higher is chattier";
	"-f", Arg.Set Common.foreground, "Stay in the foreground";
	"-l", Arg.Set Common.use_syslog, "Log to syslog instead of /tmp/lvrouted.log";
	"-p", Arg.Set_int Common.port, "UDP port to use";
	(*"-r", Arg.Set resume, "Resume from saved state"; *)
	"-s", Arg.Set_string Common.secret, "Secret to sign packets with";
	"-t", Arg.Set_string Common.tmpdir, "Temporary directory";
	"-u", Arg.Set Common.real_route_updates, "Upload routes to the kernel";
	"-v", Arg.Unit print_version, "Print version information";
]

(* This is the main function *)
let _ =
	Log.log Log.info "Starting up";

	Arg.parse argopts ignore "lvrouted";
	Log.log Log.info "Parsed commandline";

	if not !Common.foreground then begin
		LowLevel.daemon false false;
		Log.log Log.info "daemonized";
	end;

	if not !resume then begin
		read_config ();
		Log.log Log.info "Read config";
	end else begin
		read_state (Common.read_file (!Common.tmpdir ^ "lvrouted.state"));
		Log.log Log.info "Resumed from saved state";
	end;

	let set_handler f = List.iter (fun i -> Sys.set_signal i (Sys.Signal_handle f)) in
	set_handler abort_handler [Sys.sigabrt; Sys.sigquit; Sys.sigterm ];
	set_handler (fun _ -> read_config ()) [Sys.sighup];
	set_handler dump_version [Sys.sigusr1];
	set_handler dump_state [Sys.sigusr2];
	Log.log Log.info "Set signal handlers";

	let udpsockfd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
	Unix.setsockopt udpsockfd Unix.SO_REUSEADDR true;
	Unix.bind udpsockfd (Unix.ADDR_INET (Unix.inet_addr_any, !Common.port));
	let rtsockfd = LowLevel.open_rtsock () in
	Log.log Log.info "Opened and bound socket";

	let logfrom s = Log.log Log.debug
			("got data from " ^
			 Unix.string_of_inet_addr
				(Common.get_addr_from_sockaddr s)) in
	let s = String.create 65536 in
	let readfds = [ udpsockfd; rtsockfd ] in
	while true do try
		let fds, _, _ = Unix.select readfds [] []
					!Common.alarm_timeout in
		if fds = [] then
		  periodic_check udpsockfd rtsockfd
		else begin
			if List.mem udpsockfd fds then begin
				let len, sockaddr = Unix.recvfrom udpsockfd s 0
							(String.length s) [] in
				logfrom sockaddr;
				Neighbor.handle_data !neighbors
						     (String.sub s 0 len)
						     sockaddr;
				Log.log Log.debug ("data handled");
			end;
			if List.mem rtsockfd fds then
			  handle_routemsg udpsockfd rtsockfd
					  (LowLevel.read_routemsg rtsockfd);
		end;
	with _ -> ()
	done
