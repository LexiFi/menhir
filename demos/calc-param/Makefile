.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../find-menhir.sh)
endif

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := calc

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: all
	@echo "The following command should print 42.0:"
	echo "(1 + 2 * 10) * 2" | ./$(MAIN).native

