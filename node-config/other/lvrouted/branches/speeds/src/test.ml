open Tree

let tree1 = 
	{ addr = (Unix.inet_addr_of_string "172.16.123.123");
	  edges = [ { edge_bandwidth = 54;
	  	      edge_node = { addr = (Unix.inet_addr_of_string "172.16.100.100");
				    edges = []; }
		    } ] }

let _ =
	let s = Tree.to_string [ { edge_bandwidth=1000;
				   edge_node = tree1 } ] in
	let c = open_out "packet" in
	output_string c s;
	close_out c;

	let c = open_in "packet" in 
	let s = String.create 65536 in
	let len = input c s 0 65535 in
	let s = String.sub s 0 len in
	let t = Tree.from_string s (Unix.inet_addr_of_string "172.31.255.1") in
	print_string (Tree.show t.edges);
	print_newline ()
