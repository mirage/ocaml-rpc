OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = lwt,lwt.unix,oUnit,result
RPC = -I ../_build/lib
RPCLINK = rpclib_core.cmxa rpclib.cmxa

PPXOPTS=-package ppx_deriving -ppxopt ppx_deriving,../_build/ppx/ppx_deriving_rpc.cma

default: test tytest

test: test.ml
	$(OCAMLOPT) $(PPXOPTS) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -o test -dsource test.ml 2> test.gen.ml
	./test

tytest: tytest.ml
	$(OCAMLOPT) $(PPXOPTS) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -dsource -o tytest tytest.ml 2> tytest.gen.ml
	./tytest

clean:
	rm -f test tytest *.cmxa *.cmo *.cmo *.o *~ *.gen.ml
