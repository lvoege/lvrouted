(* Config file parser. *)
open Neighbor
open HopInfo
open Route
open Str

let parse_file chan =
	let direct_re s =
		Str.string_match (
			Str.regexp "^direct \\([^/]+\\)") s 0 in
	let neighbor_re s =
		Str.string_match (
			Str.regexp "^neighbor \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)") s 0 in
	
	let directs = ref [] in
	let neighbors = ref [] in
	try
		while true do
			let line = input_line chan in
			let group i = Str.matched_group i line in
			if direct_re line then begin
				let addr = Unix.inet_addr_of_string (group 1) in
				let d = HopInfo.make addr in
				directs := d::(!directs)
			end else if neighbor_re line then begin
				let name = group 1 in
				let addr = Unix.inet_addr_of_string (group 2) in
				let iface = group 3 in
				let n = Neighbor.make name iface addr in
				neighbors := n::(!neighbors)
			end;
		done;
		Array.of_list (!directs), !neighbors
	with End_of_file ->
		Array.of_list (!directs), !neighbors
