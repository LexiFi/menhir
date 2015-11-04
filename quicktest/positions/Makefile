.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../../demos/find-menhir.sh)
endif

MENHIRFLAGS     := --infer -v --fixed-exception
OCAMLBUILD      := ocamlbuild -use-ocamlfind
MAIN            := calc

all:
	cd .. && ./build.sh
# ocamlyacc.
	@ $(OCAMLBUILD) -build-dir _ocamlyacc                      $(MAIN).native
# Menhir, code back-end, %inline disabled.
	@ $(OCAMLBUILD) -build-dir _menhir_code         -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) -lc 1 --comment --no-code-inlining --no-inline"
# Menhir, code back-end, %inline enabled.
	@ $(OCAMLBUILD) -build-dir _menhir_code_inline  -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) -lc 1 --comment --no-code-inlining"
# Menhir, table back-end, %inline disabled.
	@ $(OCAMLBUILD) -build-dir _menhir_table        -tag fancy $(MAIN).native \
	  -package menhirLib -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) --table --no-inline "
# Menhir, table back-end, %inline enabled.
	@ $(OCAMLBUILD) -build-dir _menhir_table_inline -tag fancy $(MAIN).native \
	  -package menhirLib -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) --table"

clean:
	@ rm -f *~ .*~ *.log
	@ $(OCAMLBUILD) -build-dir _ocamlyacc           -clean
	@ $(OCAMLBUILD) -build-dir _menhir_code         -clean
	@ $(OCAMLBUILD) -build-dir _menhir_code_inline  -clean
	@ $(OCAMLBUILD) -build-dir _menhir_table        -clean
	@ $(OCAMLBUILD) -build-dir _menhir_table_inline -clean

# We try every input file whose name matches *.in.
# We parse it using each of the parsers,
# and compare the results pairwise.

test: all
	@ for f in *.in ; do \
	  base=$${f%*.in} ; \
	  out=$$base.out ; \
	  echo "Processing $$f..." ; \
	  for target in _ocamlyacc _menhir_code _menhir_code_inline _menhir_table _menhir_table_inline ; do \
	    $$target/$(MAIN).native < $$f > $$target/$$out ; \
	  done ; \
	done
	@ for pair in \
	  _ocamlyacc/_menhir_code \
	  _ocamlyacc/_menhir_table \
	  _menhir_code/_menhir_table \
	  _menhir_code_inline/_menhir_table_inline \
	  _menhir_table/_menhir_table_inline \
	  _menhir_code/_menhir_code_inline \
	; do \
	  left=$${pair%/*} ; \
	  right=$${pair#*/} ; \
	  for f in *.in ; do \
	    base=$${f%*.in} ; \
	    out=$$base.out ; \
	    log=$$base.$$left.$$right.log ; \
	    if diff $$left/$$out $$right/$$out > $$log ; then \
	      echo "$$left versus $$right: $$f: OK" ; \
	    else \
	      echo "$$left versus $$right: $$f: FAILURE" ; \
	      cat $$log ; \
	    fi ; \
	  done ; \
	done

