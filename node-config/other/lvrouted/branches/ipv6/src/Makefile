REQUIRES  = unix str
OCAMLC    = ocamlfind ocamlc -w p -g -linkpkg -package "$(REQUIRES)"
OCAMLC_I  = ocamlc -i
OCAMLOPT  = ocamlfind ocamlopt -w p -linkpkg -package "$(REQUIRES)"
OCAMLDEP  = ocamldep
OCAMLFIND = ocamlfind
OCAMLPROF = ocamlprof
CFLAGS= -Wall -I`ocamlfind printconf stdlib` -g
LIBS=-cclib -lcrypto

SOURCES= Version.ml LowLevel.ml Common.ml Log.ml MAC.ml Iface.ml Route.ml Tree.ml Neighbor.ml lvrouted.ml
MLIS= $(SOURCES:.ml=.mli)
BYTECODE= $(SOURCES:.ml=.cmo)
OBJCODE= $(SOURCES:.ml=.cmx)

all: depend lvrouted lvrouted.opt 

.SUFFIXES: .ml .mli .cmo .cmi .cmx

clean:
	rm -f lvrouted lvrouted.opt lowlevel_c.o $(MLIS) $(BYTECODE)
	rm -f $(OBJCODE) $(SOURCES:.ml=.o) $(SOURCES:.ml=.cmi) *~

lvrouted.opt: $(OBJCODE) lowlevel_c.o Version.cmx
	$(OCAMLOPT) -o $@ $(OBJCODE) lowlevel_c.o $(LIBS)

lvrouted: $(MLIS) $(BYTECODE) lowlevel_c.o Version.cmo
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

Version.ml: .svn/entries
	@echo Extracting svn version info
	@echo \(\* DO NOT EDIT BY HAND \*\) > Version.ml
	@echo let version=`svn info . | grep Revision | sed "s/.* //g"` >> Version.ml
	@echo let branch=\"`pwd | sed "s/.*lvrouted\///g" | sed "s/\/src//g"`\" >> Version.ml
	@echo let date=\"`date`\" >> Version.ml
	@echo let host=\"`uname -a`\" >> Version.ml
	@echo let ocamlopt=\"`ocamlopt -v | head -1`\" >> Version.ml
	@echo let who=\"`whoami`\" >> Version.ml

tags:
	otags $(SOURCES)
	ctags -a -f TAGS lowlevel_c.c

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.if exists(".depend")
include .depend
.endif
