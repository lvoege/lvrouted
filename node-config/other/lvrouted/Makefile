REQUIRES  = unix str
OCAMLC    = ocamlfind ocamlc -w p -g -linkpkg -package "$(REQUIRES)"
OCAMLC_I  = ocamlc -i
OCAMLOPT  = ocamlfind ocamlopt -w p -linkpkg -package "$(REQUIRES)"
OCAMLDEP  = ocamldep
OCAMLFIND = ocamlfind
OCAMLPROF = ocamlprof
CFLAGS= -Wall -I`ocamlfind printconf stdlib` -g
LIBS=-cclib -lbz2 -cclib -lcrypto

SOURCES= LowLevel.ml Common.ml Log.ml MAC.ml Iface.ml Route.ml Tree.ml Neighbor.ml lvrouted.ml
MLIS= $(SOURCES:.ml=.mli)
BYTECODE= $(SOURCES:.ml=.cmo)
OBJCODE= $(SOURCES:.ml=.cmx)

all: depend lvrouted lvrouted.opt

.SUFFIXES: .ml .mli .cmo .cmi .cmx

clean:
	rm -f lowlevel_c.o $(MLIS) $(BYTECODE) $(OBJCODE) $(SOURCES:.ml=.o) $(SOURCES:.ml=.cmi) *~

lvrouted.opt: $(OBJCODE) lowlevel_c.o
	$(OCAMLOPT) -o $@ $(OBJCODE) lowlevel_c.o $(LIBS)

lvrouted: $(MLIS) $(BYTECODE) lowlevel_c.o
	$(OCAMLC) -o $@ -custom lowlevel_c.o $(BYTECODE) $(LIBS)

.o: %.c
	$(CC) $(CFLAGS) -c $<

.ml.mli:
	$(OCAMLC_I) $(OCAMLFLAGS) -c $< > $@
	$(OCAMLC) $(OCAMLFLAGS) -c $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.prof:
	$(OCAMLPROF) $< > $@

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.ml.s:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -S -c $<

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.if exists(".depend")
include .depend
.endif
