.PHONY: build release install uninstall clean test reindent reformat

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

test:
	jbuilder runtest

reindent:
	git ls-files '*.ml*' | xargs ocp-indent -i

reformat:
	git ls-files '*.ml*' | xargs ocamlformat -i
