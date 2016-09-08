OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g -thread

PACKS = lwt,lwt.unix,result
RPC = -I ../_build/lib
RPCLINK = rpclib_core.cmxa rpclib.cmxa

PPXOPTS=-package ppx_deriving -ppxopt ppx_deriving,../_build/ppx/ppx_deriving_rpc.cma

default: example example2 example3

example: example.ml
	$(OCAMLOPT) $(PPXOPTS) -linkpkg -package $(PACKS) $(RPC) $(RPCLINK) -o example -dsource example.ml 2> example.gen.ml
	./example

example2: example2.ml example2_idl.ml example2_client.ml example2_server.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -linkpkg -package $(PACKS),threads,ppx_deriving_rpc,rpclib -o example2 example2_idl.ml example2_server.ml example2_client.ml example2.ml

example3: example3_idl.ml example3_client.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -linkpkg -package $(PACKS),threads,ppx_deriving_rpc,rpclib -o example3 example3_idl.ml example3_client.ml

clean:
	rm -f example example2 *.cmx *.cmi *.cmo *.o example.gen.ml *~
