.PHONY: all install

all:
	cd lib && $(MAKE) all

install:
	cd lib && $(MAKE) install

uninstall:
	cd lib && $(MAKE) uninstall

tests:
	cd tests && $(MAKE) all

clean:
	cd lib && $(MAKE) clean
	cd tests && $(MAKE) clean
