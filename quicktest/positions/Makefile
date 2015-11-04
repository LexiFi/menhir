.PHONY: test all clean

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../../demos/find-menhir.sh)
endif

MENHIRFLAGS     := --infer -v --fixed-exception
OCAMLBUILD      := ocamlbuild -use-ocamlfind
MAIN            := calc
TARGETS         := _ocamlyacc _menhir_code _menhir_code_inline _menhir_table _menhir_table_inline

# We try every input file whose name matches *.in.
# We parse it using each of the parsers,
# and compare the results pairwise.

test:
# Make sure everything is freshly compiled.
	$(MAKE) clean
	cd .. && ./build.sh
	$(MAKE) all
# Create the output files.
	@ for f in *.in ; do \
	  base=$${f%*.in} ; \
	  out=$$base.out ; \
	  echo "Processing $$f..." ; \
	  for target in $(TARGETS) ; do \
	    $$target/$(MAIN).native < $$f > $$target/$$out ; \
	  done ; \
	done
# Compare the output files. Lots of comparisons.
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

all:
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
	@ for target in $(TARGETS) ; do \
	  $(OCAMLBUILD) -build-dir $$target -clean ; \
	done
