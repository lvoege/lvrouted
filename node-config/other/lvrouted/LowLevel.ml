(* Interface to functions in lowlevel.c *)

(* For debugging purposes it's handy to be able to print a file descriptor *)
external int_of_file_descr: Unix.file_descr -> int
  = "int_of_file_descr"

(* given an address and a mask (like, for example, 24), create a bitmask and
   apply it to the address *)
external mask_addr: Unix.inet_addr -> int -> Unix.inet_addr
  = "mask_addr"

(* Return whether or not the given interface is associated or not *)
external iface_is_associated: string -> bool
  = "caml_iface_is_associated"

(* daemon(3) wrapper *)
external daemon: bool -> bool -> unit
  = "caml_daemon"

(* bzip2 the given string *)
external string_compress: string -> string
  = "string_compress"

(* bzip2 -d the given string *)
external string_decompress: string -> string
  = "string_decompress"

external ether_aton: string -> string -> bool
  = "caml_ether_aton"

external ether_ntoa: string -> string -> bool
  = "caml_ether_ntoa"

(* Wrapper around getifaddrs(3) *)
external getifaddrs: unit -> ( string		(* iface name *)
			     * int		(* iface flags *)
			     * Unix.inet_addr 	(* iface addr *)
			     * Unix.inet_addr option	(* netmask *)
			     * Unix.inet_addr option	(* broadcast *)
			     * Unix.inet_addr option)	(* dst addr *)
			     list
  = "caml_getifaddrs"

external bits_in_inet_addr: Unix.inet_addr -> int
  = "bits_in_inet_addr"

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

external get_arp_entries: unit -> (string * Unix.inet_addr * string) array
  = "get_arp_entries"

external get_associated_stations: string -> string array
  = "get_associated_stations"

external sha_string: string -> string
  = "sha_string"

external hexdump_string: string -> string
  = "hexdump_string"
