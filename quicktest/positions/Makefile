.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../../demos/find-menhir.sh)
endif

MENHIRFLAGS     := --infer
OCAMLBUILD      := ocamlbuild -use-ocamlfind
MAIN            := calc

all:
	$(OCAMLBUILD) -build-dir _ocamlyacc            $(MAIN).native
	$(OCAMLBUILD) -build-dir _menhir    -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -build-dir _ocamlyacc -clean
	$(OCAMLBUILD) -build-dir _menhir    -clean

test: all
	echo "(1 + 2 * 10) * 2" | _ocamlyacc/$(MAIN).native
	echo "(1 + 2 * 10) * 2" | _menhir/$(MAIN).native
