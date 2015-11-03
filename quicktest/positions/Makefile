.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../../demos/find-menhir.sh)
endif

MENHIRFLAGS     := --infer -v
OCAMLBUILD      := ocamlbuild -use-ocamlfind
MAIN            := calc

all:
	@ $(OCAMLBUILD) -build-dir _ocamlyacc            $(MAIN).native
	@ $(OCAMLBUILD) -build-dir _menhir    -tag fancy $(MAIN).native \
	  -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

clean:
	@ rm -f *~ .*~
	@ $(OCAMLBUILD) -build-dir _ocamlyacc -clean
	@ $(OCAMLBUILD) -build-dir _menhir    -clean

_ocamlyacc/%.out: %.in
	_ocamlyacc/$(MAIN).native < $< > $@

_menhir/%.out: %.in
	_menhir/$(MAIN).native < $< > $@

test: all
	@ for f in *.in ; do \
	  out=$${f%*.in}.out ; \
	  log=$${f%*.in}.log ; \
	  for target in _ocamlyacc _menhir ; do \
	    $$target/$(MAIN).native < $$f > $$target/$$out ; \
	  done ; \
	  if diff _ocamlyacc/$$out _menhir/$$out > $$log ; then \
	    echo "OK $$f" ; \
	  else \
	    echo "FAILURE $$f" ; \
	    cat $$log ; \
	  fi ; \
	done
