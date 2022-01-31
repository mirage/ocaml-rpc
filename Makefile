.PHONY: build release install uninstall clean test examples reindent reformat

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

test: examples
	dune runtest

examples:
	dune build @runexamples -p rpc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent -i

reformat:
	git ls-files '*.ml*' | xargs ocamlformat -i
