REQUIRES  = unix str
OCAMLC    = ocamlfind ocamlc -w p -g -linkpkg -package "$(REQUIRES)"
OCAMLC_I  = ocamlc
OCAMLOPT  = ocamlfind ocamlopt -w p -linkpkg -package "$(REQUIRES)"
OCAMLDEP  = ocamldep
OCAMLFIND = ocamlfind
CFLAGS= -Wall -I`ocamlfind printconf stdlib`

SOURCES= Common.ml LowLevel.ml Route.ml HopInfo.ml Neighbor.ml Config.ml lvrouted.ml
BYTECODE= $(SOURCES:.ml=.cmo)
OBJCODE= $(SOURCES:.ml=.cmx)

all: depend lvrouted

.SUFFIXES: .ml .mli .cmo .cmi .cmx

lvrouted.opt: $(OBJCODE) lowlevel.o
	$(OCAMLOPT) -o $@ $(OBJCODE) lowlevel.o /usr/lib/libbz2.a

lvrouted: $(BYTECODE) lowlevel.o
	$(OCAMLC) -o $@ -custom lowlevel.o $(BYTECODE) /usr/lib/libbz2.a

.o: %.c
	$(CC) $(CFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.if exists(".depend")
include .depend
.endif
