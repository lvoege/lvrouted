open Neighbor
open Common
open Route
open Config
open LowLevel
open HopInfo

let neighbors = ref []
let ifaces = ref [| |]
let direct = ref [| |]
let direct_hash = Hashtbl.create 16
let sockfd = ref Unix.stdout
let last_time = ref 0.0

let alarm_handler _ =
	(* re-trigger het alarm *)
	let _ = Unix.alarm Common.alarm_timeout in

	let nuked = Neighbor.nuke_old_hoptables !neighbors Common.timeout in
	let ifaces_flipped = ref false in
	Array.iter (fun (i, up) ->
		let up_new = LowLevel.check_iface i in
		if up != up_new then begin
		  ifaces_flipped := true;
		  if not up then
		    Neighbor.nuke_hoptable_for_iface !neighbors i;
		end) !ifaces;
	let now = Unix.time () in
	let its_time = (now -. !last_time) > bcast_interval in
	
	if nuked || !ifaces_flipped || its_time then begin
	  last_time := now;

	  (* leidt uit de huidige informatie een routetabel en een hoptable af *)
	  let routes, hoptable = Neighbor.derive_routes_and_hoptable (!direct)
	  							     direct_hash
	  							     (!neighbors) in

	  (* stuur de hoptable naar alle neighbors. Neighbor.send zal het
	     count-to-infinity probleem tegen gaan door te filteren, en heeft
	     daarbij de route tabel nodig *)
	  List.iter (Neighbor.send (!sockfd) routes hoptable) (!neighbors);

	  (* aggregeer die tabel zoveel mogelijk *)
	  let routes' = Route.aggregate routes [] in
	  let out = open_out "/tmp/lvrouted.routes" in
	  output_string out ("Route table:\n");
	  List.iter (fun r -> 
		  output_string out ("\t" ^ Route.show r ^ "\n")) routes';
	  close_out out
	end

let abort_handler _ = ()

let read_config _ =
	let d, n = Config.parse_file (open_in "lvrouted.conf") in
	direct := d;

	(* fill direct_hash *)
	Array.iter (fun e -> Hashtbl.add direct_hash (HopInfo.addr e) 1) d;

	neighbors := n;
	ifaces := Array.of_list (List.map (fun i -> i, (LowLevel.check_iface i))
					  (Neighbor.extract_ifaces n))

let main =
	read_config ();
	
	let set_handler f = List.iter (fun i -> Sys.set_signal i (Sys.Signal_handle f)) in
	set_handler alarm_handler [Sys.sigalrm];
	set_handler abort_handler [Sys.sigabrt; Sys.sigquit; Sys.sigterm ];
	set_handler (fun _ -> read_config ()) [Sys.sighup];

	(*LowLevel.daemon false false;*)

	let _  = Unix.alarm 1 in

	sockfd := Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0;
	Unix.setsockopt !sockfd Unix.SO_REUSEADDR true;
	Unix.bind !sockfd (Unix.ADDR_INET (Unix.inet_addr_any, Common.port));

	while true do 
		let s = String.create 10240 in
		try
			let _, sockaddr = Unix.recvfrom !sockfd s 0 (String.length s) [] in
			Neighbor.handle_data !neighbors s sockaddr
		with _ -> ()
	done
