.PHONY: all test clean

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../find-menhir.sh)
endif

OCAMLBUILD := ocamlbuild -use-ocamlfind
MAIN       := generate
EXECUTABLE := menhir-generate-printers

all:
	$(OCAMLBUILD) $(MAIN).native
	rm -f $(MAIN).native
	cp -f _build/$(MAIN).native $(EXECUTABLE)

TEST       := ../../bench/good/parser_raw

test:
	$(MENHIR) --cmly $(TEST).mly
	./$(EXECUTABLE) $(TEST).cmly

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean
