(* Interface to functions in lowlevel_c.c *)

(* Set hard limits on data and coredump size *)
external set_limits: int -> int -> bool
  = "set_limits" "noalloc"

(* For debugging purposes it's handy to be able to print a file descriptor. *)
external int_of_file_descr: Unix.file_descr -> int
  = "int_of_file_descr" "noalloc"

(* Given an address and a mask length (like, for example, 24), create a
   bitmask and apply it to the address *)
external mask_addr: Unix.inet_addr -> int -> Unix.inet_addr
  = "mask_addr"

(* Return whether or not the given interface is associated *)
external iface_is_associated: string -> bool
  = "caml_iface_is_associated" "noalloc"

(* daemon(3) *)
external daemon: bool -> bool -> unit
  = "caml_daemon" "noalloc"

(* bzip2 the given string. NEEDS TESTING. *)
external string_compress: string -> string
  = "string_compress"

(* bzip2 -d the given string. NEEDS TESTING. *)
external string_decompress: string -> string
  = "string_decompress"

(* ether_aton(3) *)
external ether_aton: string -> string -> bool
  = "caml_ether_aton"

(* ether_ntoa(3) *)
external ether_ntoa: string -> string -> bool
  = "caml_ether_ntoa"

(* getifaddrs(3). Will only return IPv4 addresses for now. *)
external getifaddrs: unit -> ( string			(* iface name *)
			     * int			(* iface flags *)
			     * Unix.inet_addr 		(* iface addr *)
			     * Unix.inet_addr option	(* netmask *)
			     * Unix.inet_addr option	(* broadcast *)
			     * Unix.inet_addr option)	(* dst addr *)
			     list
  = "caml_getifaddrs"

(* How many bits in the given address are set? *)
external bits_in_inet_addr: Unix.inet_addr -> int
  = "bits_in_inet_addr" "noalloc"

(* strstr(3) *)
external strstr: string -> string -> int
  = "caml_strstr"

(* Given an address and a netmask, return all the usable addresses in that
   block. So 172.16.0.1/30 will return 172.16.0.1 and 172.16.0.2. *)
external get_addrs_in_block: Unix.inet_addr -> int -> Unix.inet_addr list
  = "get_addrs_in_block"

(* Get the entire arp table. *)
external get_arp_entries: unit -> ( string		(* iface name *)
				  * Unix.inet_addr	(* IPv4 addr *)
				  * string)		(* MAC.t *)
				  list
  = "get_arp_entries"

(* Get all the MAC.t's that are associated with the interface with the given 
   name. *)
external get_associated_stations: string -> string array
  = "get_associated_stations"

(* SHA1(3) *)
external sha_string: string -> string
  = "sha_string"

(* Return a primitively hexdumped version of the given (possibly binary)
   string. Useful when debugging. *)
external hexdump_string: string -> string
  = "hexdump_string"

(* Send the given string with the given priority to the syslog. These
   priorities are those as defined in Log.ml, /not/ the standard values
   from <syslog.h> ! *)
external syslog: int -> string -> unit
  = "caml_syslog"

external pack_int: int -> string
  = "caml_pack_int"

external unpack_int: string -> int
  = "caml_unpack_int"

external open_rtsock: unit -> Unix.file_descr
  = "open_rtsock"

(* Define the routing messages. There are a lot more, but it turns out these
   are either preceded or followed by RTM_NEWADDR's or RTM_DELADDR's. For our
   purpose it's enough to listen for those two *)
type routemsg =
	  RTM_NOTHING
	| RTM_NEWADDR	of string * Unix.inet_addr * int
	| RTM_DELADDR	of string * Unix.inet_addr * int

(* RTM_NOTHING: nothing interesting
   RTM_NEWADDR: the interface with the given name got a new address and netmask
   RTM_DELADDR: the interface with the given name lost the given address and netmask
 *)
external read_routemsg: Unix.file_descr -> routemsg
  = "read_routemsg"

external compare_ipv4_addrs: Unix.inet_addr -> Unix.inet_addr -> int
  = "compare_ipv4_addrs"

external route_includes_impl: Unix.inet_addr -> int -> Unix.inet_addr -> int -> bool
	= "route_includes_impl" "noalloc"

