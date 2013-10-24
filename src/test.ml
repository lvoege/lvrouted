open Tree

let read_packet s = 
	let c = open_in s in
	let s = String.create 65535 in
	let len = input c s 0 65535 in
	close_in c;
	let s = String.sub s 24 (len - 24) in
	Tree.from_string s (Unix.inet_addr_of_string "127.0.0.1")

let _ =
	let t = read_packet "packet" in
	print_string (Tree.show [t]);
	print_newline ();
