(* Main module. A receive loop plus an alarm handler that'll do the
   sending to neighbors and the updating of the route table. *)
open Neighbor
open Common
open Route
open Config
open LowLevel
open HopInfo
open Iface

(* First some globals. these are global because the signal handlers need to be
   able to use them *)

(* A list of Neighbor.t's *)
let neighbors = ref []
(* A list of strings with interface names. 'ep0', 'sis1' etc *)
let ifaces = Hashtbl.create 16
(* An array of HopInfo.t's for every one of 'our' addresses. *)
let direct = ref [| |]
(* A hashtable to be able to quickly see if an address is one of 'our's.
   The keys are Unix.inet_addr's and the values are 1. *)
let direct_hash = Hashtbl.create 16
(* A socket file handle for everything to use. Unix.file_descr is an abstract
   type, initialize it with Unix.stdout to it typechecks, then have main
   immediately replace it with a real handle. *)
let sockfd = ref Unix.stdout
(* last broadcast timestamp *)
let last_time = ref 0.0
(* The current routing table. This starts out empty. *)
let routes = ref Route.RouteSet.empty
(* Neighbor -> 1. An entry means that neighbor was unreachable last iteration *)
let unreachable: (Neighbor.t, int) Hashtbl.t = Hashtbl.create 4

let alarm_handler _ =
	(* re-trigger het alarm *)
	let _ = Unix.alarm Common.alarm_timeout in

	let someone_became_unreachable = ref false in
	let new_unreachable = Hashtbl.create 4 in
	List.iter (fun n ->
		let iface = Hashtbl.find ifaces n.iface in
		if not (Neighbor.check_reachable n iface) then begin
		  Hashtbl.add new_unreachable n 1;
		  try
		  	let _ = Hashtbl.find unreachable n in
			()
		  with Not_found ->
		  	someone_became_unreachable := true
		end) !neighbors;

	let now = Unix.gettimeofday () in
	let its_time = (now -. !last_time) > bcast_interval in
	
	if !someone_became_unreachable || its_time then begin
	  last_time := now;

	  (* leidt uit de huidige informatie een routetabel en een hoptable af *)
	  let newroutes, hoptable = Neighbor.derive_routes_and_hoptable (!direct)
	  							     direct_hash
	  							     (!neighbors) in

	  (* stuur de hoptable naar alle neighbors. Neighbor.send zal het
	     count-to-infinity probleem tegen gaan door te filteren, en heeft
	     daarbij de route tabel nodig *)
	  List.iter (Neighbor.send (!sockfd) newroutes hoptable) (!neighbors);

	  (* aggregeer die tabel zoveel mogelijk *)
	  let routes' = Route.aggregate newroutes in
	  if Common.real_route_updates then begin
		let deletes, adds = Route.diff !routes routes' in
		Route.commit deletes adds
	  end else begin
		let out = open_out "/tmp/lvrouted.routes" in
		output_string out ("Route table:\n");
		Route.RouteSet.iter (fun r ->
			output_string out ("\t" ^ Route.show r ^ "\n")) routes';
		close_out out;

		let out = open_out "/tmp/lvrouted.neighbors" in
		List.iter (fun n ->
			output_string out (Neighbor.show n)) !neighbors;
		close_out out
	  end;
	  routes := routes'
	end

let abort_handler _ = ()

let read_config _ =
	let block_signals = [ Sys.sigalrm ] in
	let _ = Unix.sigprocmask Unix.SIG_BLOCK block_signals in

	let localaddrs = List.map (fun (_, _, a, _, _, _) -> a)
			          (Array.to_list (LowLevel.getifaddrs ())) in
	let wleidenaddrs = List.filter LowLevel.inet_addr_in_range localaddrs in
	direct := Array.of_list (List.map (fun a ->
			Hashtbl.add direct_hash a 1;
			HopInfo.make a) wleidenaddrs);
	let n = Config.parse_file (open_in "/tmp/lvrouted.conf") in
	neighbors := n;
	Neighbor.extract_ifaces ifaces n;

	let _ = Unix.sigprocmask Unix.SIG_UNBLOCK block_signals in
	()

let main =
	read_config ();

	let set_handler f = List.iter (fun i -> Sys.set_signal i (Sys.Signal_handle f)) in
	set_handler alarm_handler [Sys.sigalrm];
	set_handler abort_handler [Sys.sigabrt; Sys.sigquit; Sys.sigterm ];
	set_handler (fun _ -> read_config ()) [Sys.sighup];

	(*LowLevel.daemon false false; *)

	let _  = Unix.alarm 1 in

	sockfd := Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0;
	Unix.setsockopt !sockfd Unix.SO_REUSEADDR true;
	Unix.bind !sockfd (Unix.ADDR_INET (Unix.inet_addr_any, Common.port));

	let s = String.create 10240 in
	while true do 
		try
			let _, sockaddr = Unix.recvfrom !sockfd s 0 (String.length s) [] in
			Neighbor.handle_data !neighbors s sockaddr
		with _ -> ()
	done
