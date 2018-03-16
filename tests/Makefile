OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = unix,rresult,xmlm
RPC = -I ../_build/lib
RPCLINK = rpclib.cmxa rpclib_core.cmxa json.cmxa xml.cmxa

CAMLP4 = camlp4o $(shell ocamlfind query type_conv -r -format "-I %d %a" -predicates syntax,preprocessor) $(RPC) rpclib_syntax.cma
PPXOPTS=-package ppx_deriving -ppxopt ppx_deriving,../_build/ppx/ppx_deriving_rpc.cma

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
	client_lwt \
	client_lwt_new \
	client_async_new


EXECS=$(foreach example, $(EXAMPLES), $(example).opt)

.PHONY: all clean
all: $(EXECS)
	make $(EXECS:%=%.run)

%.run: %
	./$?

client.opt: idl_test.cmx idl_test.cmi client.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -o $@ idl_test.cmx client.cmx

client_lwt.opt: idl_test.cmx idl_test.cmi client_lwt.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS),lwt,lwt.unix $(RPC) $(RPCLINK) rpclib_lwt.cmxa -o $@ idl_test.cmx client_lwt.cmx

idl_test.cmx: idl_test.ml idl_test.cmi
	$(OCAMLOPT) -package $(PACKS) $(RPC) $(RPCLINK) idl.cmxa -pp "camlp4o $(RPC) idl.cma" -c -o $@ $<

client_lwt_new.opt: client_lwt_new.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS),lwt,lwt.unix $(RPC) $(RPCLINK) rpclib_lwt.cmxa -o $@ client_lwt_new.cmx

client_async_new.opt: client_async_new.cmx
	$(OCAMLOPT) -linkpkg -thread -package $(PACKS),async $(RPC) $(RPCLINK) rpclib_async.cmxa -o $@ client_async_new.cmx

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -o $@ $<

client_lwt.cmx: client_lwt.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),lwt,lwt.unix $(RPC) $(RPCLINK) rpclib_lwt.cmxa -c -o $@ $<

client_lwt_new.cmx: client_lwt_new.ml
	$(OCAMLOPT) $(PPXOPTS) $(OCAMLFLAGS) -package $(PACKS),lwt,lwt.unix $(RPC) $(RPCLINK) rpclib_lwt.cmxa -c -o $@ $<

client_async_new.cmx: client_async_new.ml
	$(OCAMLOPT) $(PPXOPTS) $(OCAMLFLAGS) -package threads,$(PACKS),async $(RPC) $(RPCLINK) -thread -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) $(RPC) $(RPCLINK) -pp "$(CAMLP4)" -c -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) $(RPC) $(RPCLINK) -pp "$(CAMLP4)" -c -o $@ $<

%_gen: %.ml
	$(CAMLP4) $< -printer o > $@.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) $(RPC) $(RPCLINK) -c -o $@ $@.ml

idl_test_gen.ml: idl_test.ml
	camlp4o $(RPC) idl.cma  $< -printer o > $@


clean:
	rm -f *_gen.ml *.annot *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)
