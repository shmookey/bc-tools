
all: .stack-work
	mkdir -p build
	stack build --copy-bins --local-bin-path build/

.stack-work:
	stack setup

install:
	cp build/arc /usr/local/bin/arc
	cp build/jrc /usr/local/bin/jrc

clean:
	stack clean
	rm -rf build/

