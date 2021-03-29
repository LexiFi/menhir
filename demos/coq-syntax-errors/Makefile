include Makefile.extr

MENHIR            := ../../_build/install/default/bin/menhir
MENHIRFLAGS       := --coq --coq-no-version-check

all: Parser.v extraction/Parser.ml .depend calc

test: all
	./test.sh

Parser.v : Parser.vy
	$(MENHIR) $(MENHIRFLAGS) $<

extraction/Parser.ml: Parser.v
	coqc -Q . MiniCalc Parser.v
	coqtop -Q . MiniCalc -batch -load-vernac-source Extraction.v

ParserMessages.ml: parserMessages.messages Parser.vy
	$(MENHIR) $(MENHIRFLAGS) Parser.vy --compile-errors parserMessages.messages > ParserMessages.ml

include Makefile.messages.maintenance

.PHONY: clean
clean::
	@ rm -f `cat .gitignore`
