MENHIR            := dune exec menhir --
MENHIRFLAGS       := --coq --coq-no-version-check

# [make all] compiles this demo.

# We first generate Parser.v and ParserMessages.ml (via two separate
# invocations of menhir), then generate extraction/*.ml (by invoking
# Coq), then compile the OCaml code (by using dune).

.PHONY: all
all: Parser.v ParserMessages.ml extraction/Parser.ml
	dune build .

# [make test] tests just this demo.

.PHONY: test
test: all
	dune build --force --display short @essai

# [make clean] cleans up just this demo.

.PHONY: clean
clean::
	@ rm -rf `cat .gitignore`
	@ $(MAKE) -f ../Makefile.demo $@

# Auxiliary targets.

Parser.v: Parser.vy
	$(MENHIR) $(MENHIRFLAGS) $<

ParserMessages.ml: parserMessages.messages Parser.vy
	$(MENHIR) $(MENHIRFLAGS) Parser.vy --compile-errors parserMessages.messages > ParserMessages.ml

extraction/Parser.ml: Parser.v
	coqc -Q . MiniCalc Parser.v
	coqtop -Q . MiniCalc -batch -load-vernac-source Extraction.v

# Rules for maintaining the .messages file.

include Makefile.messages.maintenance
