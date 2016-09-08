OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = rpclib,lwt,lwt.unix,rpclib.json
EXAMPLES = \
	all_types \
	phantom \
	xapi \
	json \
	option \
	encoding \
	dict \
	variants \
	client \
	client_lwt

EXECS=$(foreach example, $(EXAMPLES), $(example).opt)

.PHONY: all clean
all: $(EXECS)
	make $(EXECS:%=%.run)

%.run: %
	./$?

client.opt: idl_test.cmx idl_test.cmi client.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ idl_test.cmx client.cmx

client_lwt.opt: idl_test.cmx idl_test.cmi client_lwt.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ idl_test.cmx client_lwt.cmx
idl_test.cmx: idl_test.ml idl_test.cmi
	$(OCAMLOPT) -package $(PACKS),rpclib.idl -syntax camlp4o -c -o $@ $<

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),rpclib.syntax -syntax camlp4o -c -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),rpclib.syntax -syntax camlp4o -c -o $@ $<

%_gen: %.ml
	camlp4o $(shell ocamlfind query rpclib.syntax -r -format "-I %d %a" -predicates syntax,preprocessor) $< -printer o > $@.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $@.ml

idl_test_gen.ml: idl_test.ml
	camlp4o $(shell ocamlfind query rpclib.idl -r -format "-I %d %a" -predicates syntax,preprocessor) $< -printer o > $@


clean:
	rm -f *_gen.ml *.annot *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)
