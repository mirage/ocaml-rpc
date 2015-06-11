.PHONY: all install

HAVE_JS_OF_OCAML ?= $(shell if ocamlfind query js_of_ocaml > /dev/null 2>&1; then echo true; else echo false; fi)

all:
	cd lib && $(MAKE) all
ifeq ($(HAVE_JS_OF_OCAML),true)
	cd lib && $(MAKE) all-js
endif

install:
ifeq ($(HAVE_JS_OF_OCAML),true)
	cd lib && $(MAKE) install-js
else
	cd lib && $(MAKE) install
endif

uninstall:
	cd lib && $(MAKE) uninstall

.PHONY: tests
tests:
	cd tests && $(MAKE) all

clean:
	cd lib && $(MAKE) clean
	cd tests && $(MAKE) clean

VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
NAME    = $(shell grep 'Name:' _oasis    | sed 's/Name: *//')
ARCHIVE = https://github.com/mirage/ocaml-rpc/archive/v$(VERSION).tar.gz

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push upstream v$(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	OPAMYES=1 opam publish submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)
