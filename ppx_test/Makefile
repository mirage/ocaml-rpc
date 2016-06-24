OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = lwt,lwt.unix,oUnit,result
RPC = -I ../_build/lib
RPCLINK = rpclib_core.cmxa rpclib.cmxa

EXAMPLES = \
	all_types \
	test \
	phantom \
	xapi \
	json \
	option \
	encoding \
	dict \
	variants \
	client \
	client_lwt

PPXOPTS=-package ppx_deriving -ppxopt ppx_deriving,../_build/ppx/ppx_deriving_rpc.cma

test: test.ml
	$(OCAMLOPT) $(PPXOPTS) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -o test test.ml
	./test

clean:
	rm -f test
