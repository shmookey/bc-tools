
all:
	mkdir -p build
	stack setup
	stack build --copy-bins --local-bin-path build/

install:
	cp build/arc /usr/local/bin/arc

