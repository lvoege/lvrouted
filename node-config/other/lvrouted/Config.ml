(* Config file parser. *)
open Neighbor
open HopInfo
open Route
open Str

let parse_file chan =
	let neighbor_re s =
		Str.string_match (
			Str.regexp "^neighbor \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)") s 0 in
	
	let neighbors = ref [] in
	try
		while true do
			let line = input_line chan in
			let group i = Str.matched_group i line in
			if neighbor_re line then begin
				let name = group 1 in
				let addr = Unix.inet_addr_of_string (group 2) in
				let iface = group 3 in
				let n = Neighbor.make name iface addr in
				neighbors := n::(!neighbors)
			end;
		done;
		!neighbors
	with End_of_file ->
		!neighbors
