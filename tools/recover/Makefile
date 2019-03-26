.PHONY: all clean

all:
	dune build src/main.bc

clean:
	dune clean

test:
	dune build src/main.bc demo/calc.bc
	_build/default/src/main.bc _build/default/demo/parser.cmly
