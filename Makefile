.PHONY: build release install uninstall clean test reindent reformat

build:
	dune build @install

release:
	dune build @install --profile=release

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest

reindent:
	git ls-files '*.ml*' | xargs ocp-indent -i

reformat:
	git ls-files '*.ml*' | xargs ocamlformat -i
