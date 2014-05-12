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
