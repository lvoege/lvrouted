
let from_ = ref Unix.inet_addr_any
let to_ = ref Unix.inet_addr_any
let numpackets = ref 1000

let argopts =
	let snarf r s = r := Unix.inet_addr_of_string s in [
	"-f", Arg.String (snarf from_), "From address";
	"-t", Arg.String (snarf to_), "UDP port to use";
	"-c", Arg.Set_int numpackets, "number of packets";
]

let main =
	Arg.parse argopts ignore "crashme";

	let sockfd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
	Unix.setsockopt sockfd Unix.SO_REUSEADDR true;
	Unix.bind sockfd (Unix.ADDR_INET (!from_, !Common.port));

	let randfd = open_in "/dev/urandom" in

	Random.self_init ();
	let s = String.create 65536 in
	let tosock = Unix.ADDR_INET (!to_, !Common.port) in
	for i = 0 to !numpackets - 1 do
		let len = input randfd s 0 (Random.int 65535) in
		let s' = Marshal.to_string (String.sub s 0 len) [] in
		try ignore(Unix.sendto sockfd s' 0 (String.length s') [] tosock)
		with _ -> ()
	done
