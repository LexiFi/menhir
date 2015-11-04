.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../../demos/find-menhir.sh)
endif

MENHIRFLAGS     := --infer -v --fixed-exception
OCAMLBUILD      := ocamlbuild -use-ocamlfind
MAIN            := calc

all:
	@ $(OCAMLBUILD) -build-dir _ocamlyacc               $(MAIN).native
	@ $(OCAMLBUILD) -build-dir _menhir       -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) -lc 1 --comment --no-code-inlining"
	@ $(OCAMLBUILD) -build-dir _menhir_table -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) --table" -package menhirLib
	@ $(OCAMLBUILD) -build-dir _menhir_no_inline -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) --table --no-inline" -package menhirLib

clean:
	@ rm -f *~ .*~
	@ $(OCAMLBUILD) -build-dir _ocamlyacc        -clean
	@ $(OCAMLBUILD) -build-dir _menhir           -clean
	@ $(OCAMLBUILD) -build-dir _menhir_table     -clean
	@ $(OCAMLBUILD) -build-dir _menhir_no_inline -clean

# We try every input file whose name matches *.in.
# We parse it using each of the parsers,
# and compare the results pairwise.

test: all
	@ for f in *.in ; do \
	  out=$${f%*.in}.out ; \
	  log=$${f%*.in}.log ; \
	  for target in _ocamlyacc _menhir _menhir_table _menhir_no_inline ; do \
	    $$target/$(MAIN).native < $$f > $$target/$$out ; \
	  done ; \
	  if diff _ocamlyacc/$$out _menhir/$$out > $$log ; then \
	    echo "ocamlyacc versus menhir(code): $$f: OK" ; \
	  else \
	    echo "ocamlyacc versus menhir(code): $$f: FAILURE" ; \
	    cat $$log ; \
	  fi ; \
	  if diff _ocamlyacc/$$out _menhir_table/$$out > $$log ; then \
	    echo "ocamlyacc versus menhir(table): $$f: OK" ; \
	  else \
	    echo "ocamlyacc versus menhir(table): $$f: FAILURE" ; \
	    cat $$log ; \
	  fi ; \
	  if diff _menhir/$$out _menhir_table/$$out > $$log ; then \
	    echo "menhir(code) versus menhir(table): $$f: OK" ; \
	  else \
	    echo "menhir(code) versus menhir(table): $$f: FAILURE" ; \
	    cat $$log ; \
	  fi ; \
	  if diff _menhir_table/$$out _menhir_no_inline/$$out > $$log ; then \
	    echo "menhir(table) versus menhir(no-inline): $$f: OK" ; \
	  else \
	    echo "menhir(table) versus menhir(no-inline): $$f: FAILURE" ; \
	    cat $$log ; \
	  fi ; \
	done

