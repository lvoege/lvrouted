(* Interface to functions in lowlevel_c.c *)

(* For debugging purposes it's handy to be able to print a file descriptor *)
external int_of_file_descr: Unix.file_descr -> int
  = "int_of_file_descr"

(* Given an address and a mask (like, for example, 24), create a bitmask and
   apply it to the address *)
external mask_addr: Unix.inet_addr -> int -> Unix.inet_addr
  = "mask_addr"

(* Return whether or not the given interface is associated *)
external iface_is_associated: string -> bool
  = "caml_iface_is_associated"

(* daemon(3) *)
external daemon: bool -> bool -> unit
  = "caml_daemon"

(* bzip2 the given string *)
external string_compress: string -> string
  = "string_compress"

(* bzip2 -d the given string *)
external string_decompress: string -> string
  = "string_decompress"

(* ether_aton(3) *)
external ether_aton: string -> string -> bool
  = "caml_ether_aton"

(* ether_ntoa(3) *)
external ether_ntoa: string -> string -> bool
  = "caml_ether_ntoa"

(* getifaddrs(3) *)
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
  = "bits_in_inet_addr"

(* strstr(3) *)
external strstr: string -> string -> int
  = "caml_strstr"

(* HIGHLY WirelessLeiden SPECIFIC: is the given address in the 172.16.0.0/12
   range we route? *)
external inet_addr_in_range: Unix.inet_addr -> bool
  = "inet_addr_in_range"

(* Given an address and a netmask, return all the usable addresses in that
   block *)
external get_addrs_in_block: Unix.inet_addr -> int -> Unix.inet_addr list
  = "get_addrs_in_block"

(* Get the entire arp table. *)
external get_arp_entries: unit -> ( string		(* iface name *)
				  * Unix.inet_addr	(* IPv4 addr *)
				  * string)		(* MAC.t *)
				  array
  = "get_arp_entries"

(* Get all the MAC.t's that are associated with the interface with the given 
   name *)
external get_associated_stations: string -> string array
  = "get_associated_stations"

(* SHA1(3) *)
external sha_string: string -> string
  = "sha_string"

external hexdump_string: string -> string
  = "hexdump_string"

(* Send the given string with the given priority to the syslog. These
   priorities are those as defined in Log.ml *)
external syslog: int -> string -> unit
  = "caml_syslog"

(* THIS IS FOR WHEN FREEBSD'S ROUTING SOCKET STARTS DELIVERING 802.11 EVENTS
type routemsg =
	  RTM_NOTHING
	| RTM_NEWADDR	of string * Unix.inet_addr * int
	| RTM_DELADDR	of string * Unix.inet_addr * int
	| RTM_IFINFO	of string * bool
	| RTM_IFANNOUNCE of string * bool
	| RTM_IEEE80211 of string

(* RTM_NOTHING: nothing interesting
   RTM_NEWADDR: the interface with the given name got a new address and netmask
   RTM_DELADDR: the interface with the given name lost the given address and netmask
   RTM_IFINFO: the given interface became active/inactive
   RTM_IFANNOUNCE: the given interface was added to the system
   RTM_IEEE80211: a wireless event *)

external read_routemsg: Unix.file_descr -> routemsg
  = "read_routemsg" *)

