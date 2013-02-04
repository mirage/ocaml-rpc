OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g
PACKS = xmlm,lwt,js_of_ocaml,js_of_ocaml.syntax

ICAMLP4 = -I $(shell ocamlfind query camlp4) \
	-I $(shell ocamlfind query type_conv)    \
	-I $(shell ocamlfind query js_of_ocaml)  \
	-I $(shell ocamlfind query lwt)

DOCDIR  = /myrepos/xen-api-libs.hg/doc
TARGETS = \
	rpc.cmi rpc.cmo rpc.o rpc.cmx \
	pa_rpc.cma idl.cma \
	xmlrpc.cmi xmlrpc.cmo xmlrpc.o xmlrpc.cmx \
	jsonrpc.cmi jsonrpc.cmo jsonrpc.o jsonrpc.cmx \
	rpc_client.cmi rpc_client.cmo rpc_client.o rpc_client.cmx \
	rpc_client_js.cmi rpc_client_js.cmo rpc_client_js_helper.cmo \
	rpc_client.cmo rpc_client_js.cma

.PHONY: all clean
all: $(TARGETS)

pa_module_conv.cmo: pa_module_conv.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4 -pp "camlp4orf" -I $(shell ocamlfind query camlp4) -o $@ $^

pa_rpc.cma: pa_module_conv.cmo rpc.cmo p4_rpc.cmo pa_rpc.cmo
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

idl.cma: p4_rpc.cmo p4_idl.cmo 
	$(OCAMLC) -a $(ICAMLP4) -o $@ $^

pa_rpc.cmo: pa_rpc.ml p4_rpc.cmo pa_module_conv.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type_conv -pp "camlp4orf" $(ICAMLP4) $@ $<

p4_rpc.cmo: p4_rpc.ml rpc.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type_conv -pp "camlp4orf" $(ICAMLP4) $@ $<

p4_idl.cmo: p4_idl.ml p4_rpc.cmo
	$(OCAMLC) $(OCAMLFLAGS) -c -package camlp4,type_conv -pp "camlp4orf" $(ICAMLP4) $@ $<

rpc_client_js.cma: rpc_client_js_helper.cmo rpc_client_js.cmo
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -syntax camlp4o $(ICAMLP4) -a -o $@ $^

rpc_client_js_helper.cmo: rpc_client_js_helper.ml
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -syntax camlp4o $(ICAMLP4) -c $@ $<

rpc_client_js.cmo: rpc_client_js.ml rpc_client_js_helper.cmo 
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -syntax camlp4o $(ICAMLP4) -c $@ $<

%.o %.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

%.cmi: %.mli %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $<

.PHONY: install
install: INSTALL_PATH = $(DESTDIR)$(shell ocamlfind printconf destdir)
install: all
	ocamlfind install -destdir "$(INSTALL_PATH)" rpclib META $(TARGETS)

.PHONY: uninstall
uninstall:
	ocamlfind remove rpclib

.PHONY: doc
doc: $(INTF)
	python ../doc/doc.py $(DOCDIR) "rpclib" "package" "jsonrpc pa_rpc rpc xmlrpc" "." "xmlm" ""

clean:
	rm -f *.cmo *.cmx *.cma *.cmxa *.annot *.o *.cmi *.a
