(* Main module. A receive loop plus an alarm handler that'll do the
   sending to neighbors and the updating of the route table. *)
open Common
open Log
open Neighbor

(* First some globals. these are global because the signal handlers need to be
   able to use them *)

(* A list of Neighbor.t's *)
let neighbors : Neighbor.t list ref = ref []
(* A list of strings with interface names. 'ep0', 'sis1' etc *)
let ifaces = ref StringMap.empty
(* A list of Tree.nodes's for every one of 'our' addresses. *)
let direct : Tree.node list ref = ref []
(* A set of address, netmask tuples *)
let directnets : (Unix.inet_addr * int) list ref = ref []
(* A socket file handle for everything to use. Unix.file_descr is an abstract
   type, initialize it with Unix.stdout to it typechecks, then have main
   immediately replace it with a real handle. *)
let sockfd = ref Unix.stdout
(* last broadcast timestamp *)
let last_time = ref 0.0
(* The current routing table. This starts out empty. *)
let routes = ref Route.Set.empty
(* Neighbor -> 1. An entry means that neighbor was unreachable last iteration *)
let unreachable = ref (Hashtbl.create 4)

let alarm_handler _ =
	Log.log Log.debug "in alarm_handler";

	(* re-trigger het alarm *)
	ignore(Unix.alarm !Common.alarm_timeout);

	(* See if anything previously reachable now isn't *)
	let someone_became_unreachable = ref false in
	let new_unreachable = Hashtbl.create 4 in
	List.iter (fun n ->
		Log.log Log.debug ("looking at " ^ (Neighbor.name n));
		let iface = StringMap.find (Neighbor.iface n) !ifaces in
		if not (Neighbor.check_reachable n iface) then begin
		  Hashtbl.add new_unreachable n 1;
		  try
		  	ignore(Hashtbl.find !unreachable n)
		  with Not_found ->
		  	Log.log Log.info ((Neighbor.name n) ^ " became unreachable");
		  	someone_became_unreachable := true
		end) !neighbors;
	unreachable := new_unreachable;

	let expired = Neighbor.nuke_old_trees !neighbors Common.timeout in

	let now = Unix.gettimeofday () in
	let its_time = (now -. !last_time) > !bcast_interval in

	(* If there's enough reason, derive a new routing table and a new
	   hop table and send it around. *)
	if !someone_became_unreachable || expired || its_time then begin
	  Log.log Log.debug "starting broadcast run";
	  last_time := now;

	  List.iter (fun n ->
	  	let fname = "/tmp/lvrouted.tree-" ^ n.name in
	  	if Common.is_some n.tree then begin
			let out = open_out ("/tmp/lvrouted.tree-" ^ n.name) in
			output_string out (Tree.show [Common.from_some n.tree]);
			close_out out
		end else if Sys.file_exists fname then Sys.remove fname) !neighbors;

	  let newroutes, nodes =
		Neighbor.derive_routes_and_hoptable !directnets
						    !neighbors in
	  let nodes' = List.append nodes !direct in 

	  let out = open_out ("/tmp/lvrouted.mytree") in
	  output_string out (Tree.show nodes');
	  close_out out;

	  List.iter (Neighbor.send (!sockfd) nodes') (!neighbors);

	  if !Common.real_route_updates then begin
		let deletes, adds = Route.diff !routes newroutes in
		Log.log Log.info "Deletes:";
		List.iter (fun r -> Log.log Log.info (Route.show r)) deletes;
		Log.log Log.info "Adds:";
		List.iter (fun r -> Log.log Log.info (Route.show r)) adds;
		try
			Route.commit deletes adds
		with Failure s ->
			Log.log Log.errors ("Couldn't update routing table: " ^ s)
	  end else begin
		let out = open_out "/tmp/lvrouted.routes" in
		output_string out (Route.showroutes newroutes);
		close_out out;
	  end;
	  routes := newroutes; 
	  Log.log Log.debug "finished broadcast run";
	end

let abort_handler _ =
	Log.log Log.warnings "Exiting";
	exit 0

let read_config _ =
	(* Block the alarm signal while we're meddling with globals. *)
	let block_signals = [ Sys.sigalrm ] in
	let _ = Unix.sigprocmask Unix.SIG_BLOCK block_signals in

	(* Get all routable addresses that also have a netmask *)
	
	let routableaddrs = List.filter (fun (_, _, a, n, _, _) ->
			LowLevel.inet_addr_in_range a &&
			Common.is_some n) (Array.to_list (LowLevel.getifaddrs())) in
	
	(* Construct the direct and directnets sets. *)
	let direct', directnets' =
		List.fold_left
			(fun (direct, nets) (_, _, a, n, _, _) ->
				(Tree.make a)::direct,
				(a, LowLevel.bits_in_inet_addr (Common.from_some n))::nets)
			([], []) routableaddrs in
	direct := direct';
	directnets := directnets';

	(* Interlinks can be recognised by routable addresses and a netmask geq 28. *)
	let interlinks =
		List.filter (fun (_, _, _, n, _, _) ->
			LowLevel.bits_in_inet_addr
				(Common.from_some n) >= Common.interlink_netmask) routableaddrs in
	(* From the eligible interlinks, create a list of (interface name, address) tuples for all
	   the usable addresses not our own in the interlink blocks. *)
	let neighboraddrs = List.concat (
		List.map (fun (iface, _, a, n, _, _) ->
			let m = LowLevel.bits_in_inet_addr (Common.from_some n) in
			let addrs = LowLevel.get_addrs_in_block a m in
			let addrs' = List.filter (fun a' ->
				compare a' a != 0) (Array.to_list addrs) in
			List.map (fun a -> iface, a) addrs') interlinks) in

	let ifaces', neighbors' = List.fold_left (fun (ifacemap, neighbors) (iface, a) ->
			let name = Unix.string_of_inet_addr a in
			Log.log Log.debug ("neighbor " ^ name ^ " on " ^ iface);
			StringMap.add iface (Iface.make iface) ifacemap,
			((Neighbor.make name iface a)::neighbors)) (StringMap.empty, []) neighboraddrs in
	ifaces := ifaces';
	neighbors := neighbors';

	ignore(Unix.sigprocmask Unix.SIG_UNBLOCK block_signals)

let argopts = [
	"-d", Arg.Set_int Log.loglevel, "Loglevel. Higher is chattier";
	"-p", Arg.Set_int Common.port, "UDP port to use";
	"-b", Arg.Set_float Common.bcast_interval, "Interval between contacting neighbors";
	"-a", Arg.Set_int Common.alarm_timeout, "Interval between checking for interesting things";
	"-f", Arg.Set Common.foreground, "Stay in the foreground";
	"-u", Arg.Set Common.real_route_updates, "Upload routes to the kernel";
	"-s", Arg.Set_string Common.secret, "Secret to sign packets with";
]

let main =
	Log.log Log.info "Starting up";

	Arg.parse argopts (fun _ -> ()) "lvrouted";
	Log.log Log.info "Parsed commandline";

	if not !Common.foreground then begin
		LowLevel.daemon false false;
		Log.log Log.info "daemonized";
	end;

	if !Common.real_route_updates then begin
		Route.flush ();
		Log.log Log.info "Flushed routes";
	end;

	read_config ();
	Log.log Log.info "Read config";

	let set_handler f = List.iter (fun i -> Sys.set_signal i (Sys.Signal_handle f)) in
	set_handler alarm_handler [Sys.sigalrm];
	set_handler abort_handler [Sys.sigabrt; Sys.sigquit; Sys.sigterm ];
	set_handler (fun _ -> read_config ()) [Sys.sighup];
	Log.log Log.info "Set signal handlers";

	ignore(Unix.alarm 1);
	Log.log Log.info "Triggered the alarm";

	sockfd := Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0;
	Unix.setsockopt !sockfd Unix.SO_REUSEADDR true;
	Unix.bind !sockfd (Unix.ADDR_INET (Unix.inet_addr_any, !Common.port));

	Log.log Log.info "Opened and bound socket";

	let s = String.create 10240 in
	while true do 
		try
			let len, sockaddr = Unix.recvfrom !sockfd s 0 (String.length s) [] in
			Log.log Log.debug ("got data from " ^
					Unix.string_of_inet_addr (
					Common.get_addr_from_sockaddr
					sockaddr));
			Neighbor.handle_data !neighbors (String.sub s 0 len) sockaddr;
			Log.log Log.debug ("data handled");
		with _ -> ()
	done
