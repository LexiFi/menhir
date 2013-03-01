all: bench

SRC =	benchTypes.ml \
		settings.ml \
	    log.ml \
	    util.ml  \
	    parsersDB.ml \
	    generatorsDB.ml \
	    generatedParser.ml \
		inputs.ml \
		traces.ml \
		parseResult.mli \
		parseResult.ml \
		traceDiff.mli \
		traceDiff.ml \
		commands.ml\
	    actions.ml \
	    plot.ml \
	    report.ml \
	    bench.ml

bench: $(SRC)
	ocamlfind ocamlc -g -o $@ unix.cma $^ -linkpkg -package xml-light
	cp bench ../bench

clean:
	rm -f bench
	rm -f *.cm[iox] *.o

.PHONY: clean
