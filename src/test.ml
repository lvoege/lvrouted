open Tree

let read_packet s = 
	let c = open_in s in
	let s = String.create 65535 in
	let len = input c s 0 65535 in
	let s = String.sub s 24 (len - 24) in
	Tree.from_string s (Unix.inet_addr_of_string "172.31.255.1") 100

let _ =
	let packets = [
		       "172.16.2.34";
		       "172.16.2.35";
		       "172.16.2.36" ] in
	let trees = List.map (fun s -> read_packet ("lvrouted.packet-" ^ s)) packets in
	let propagate a n =
		let result = min a (Tree.bandwidth n) in
(*
		print_string ("propagate " ^ string_of_int a ^ " " ^
				string_of_int (Tree.bandwidth n));
		print_newline ();
*)
		result in
	let priority a depth =
		let res = (float_of_int a) /. (float_of_int (depth + 1)) in
(*
		print_string ("priority = " ^ string_of_float res);
		print_newline ();
*)
		res in
	let init_payload n =
		let result = (Tree.bandwidth n) in
(*
		print_string ("bw " ^ Unix.string_of_inet_addr (Tree.addr n) ^ " = " ^
				string_of_int (Tree.bandwidth n));
		print_newline ();
*)
		result in
	let nodes, routes = Tree.merge trees [] propagate priority init_payload in
	print_string (Tree.show nodes);
	print_newline ()
