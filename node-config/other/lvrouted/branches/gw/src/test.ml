open Tree

let read_packet s = 
	let c = open_in s in
	let s = String.create 65535 in
	let len = input c s 0 65535 in
	close_in c;
	let s = String.sub s 24 (len - 24) in
	Tree.from_string s (Unix.inet_addr_of_string "172.31.255.1")

let _ =
	let trees = List.fold_left (fun a em ->
		(read_packet ("dump/lvrouted.packet-172." ^ em))::a) [] [
		"16.4.121";
		"16.4.25";
		"16.4.9";
	] in
	let foo = Tree.merge trees [] in
	let nodes, routes = foo in
	Gc.compact ();
	print_string (Tree.show nodes);
	Gc.compact ();
	print_newline ()
(*
	let a = Array.make 1001 (Tree.merge trees []) in
	for i = 0 to 1000 do
		Gc.compact ();
		let trees = List.fold_left (fun a em ->
			(read_packet ("dump/lvrouted.packet-172." ^ em))::a) [] [
			"16.4.121";
			"16.4.25";
			"16.4.9";
		] in
		Gc.compact ();
		let foo = Tree.merge trees [] in
		Array.set a i foo;
		let nodes, routes = foo in
		Gc.compact ();
		print_string (Tree.show nodes);
		Gc.compact ();
		print_newline ()
	done
	*)
