(* Interface to functions in lowlevel.c *)

(* For debugging purposes it's handy to be able to print a file descriptor *)
external int_of_file_descr: Unix.file_descr -> int
  = "int_of_file_descr"

external mask_addr: Unix.inet_addr -> int -> Unix.inet_addr
  = "mask_addr"

external check_iface: string -> bool
  = "caml_check_iface"
 
external daemon: bool -> bool -> unit
  = "caml_daemon"

external valaddr: Unix.inet_addr -> unit
  = "caml_valaddr"

external string_compress: string -> string
  = "string_compress"

external string_decompress: string -> string
  = "string_decompress"
