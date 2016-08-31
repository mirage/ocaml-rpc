OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = lwt,lwt.unix,result
RPC = -I ../_build/lib
RPCLINK = rpclib_core.cmxa rpclib.cmxa

PPXOPTS=-package ppx_deriving -ppxopt ppx_deriving,../_build/ppx/ppx_deriving_rpc.cma

default: example

example: example.ml
	$(OCAMLOPT) $(PPXOPTS) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -o example -dsource example.ml 2> example.gen.ml
	./example

clean:
	rm -f example *.cmx *.cmi *.cmo *.o example.gen.ml *~
