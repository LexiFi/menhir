.PHONY: clean test expected

# Note that there is potential confusion between src/_stage1/menhir
# and src/_stage2/menhir. Here, we use the latter (built by "make
# bootstrap" in the src/ directory). But typing "make" in the src/
# directory re-builds only the former.

SHELL=/bin/bash
SRC=../../src
MENHIR=$(SRC)/_stage2/menhir.native --dump --explain --stdlib $(SRC)
CAMLC           := $(shell if ocamlfind ocamlc -v >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamlc ; \
		       elif ocamlc.opt -v >/dev/null 2>&1 ; \
                       then echo ocamlc.opt ; \
		       else echo ocamlc ; fi)

# Some tests involve several input files. The naming convention for
# these tests is /name/-[1-9].mly.
TESTS=$(shell ls | egrep '*([1]|[^2-9]).mly')
RESULTS=$(TESTS:.mly=.result)

test: clean $(RESULTS)
	@ if test -e warnings; then \
	  echo WARNINGS: ; \
	  cat warnings; \
	fi
	@ if test -e failures; then \
	  echo FAILURES: ; \
	  cat failures;	\
	  exit 1; \
	fi

# The file %.result   stores the output of menhir --only-preprocess.
# The file %.expected stores its expected output.
# The file %.out      stores the output of menhir.
#   (There is presently no expected output file.)

%.result: %.mly
	@ if echo $< | grep -e "-1.mly" &> /dev/null; then		\
	  export BASE=`echo $< | sed s/"-1.mly"/""/`;			\
	  export FILES=`echo $$BASE-[1-9].mly`;				\
	  export CMD="$$FILES --base $$BASE";				\
	else								\
	  export FILES="$<";						\
	  export BASE=`echo $< | sed s/".mly"/""/`;			\
	  export CMD="$<";						\
	fi;								\
	$(SHELL) -c '$(MENHIR) --only-preprocess $$CMD >& $@';		\
	if [ x$$CREATE_EXPECTED == x"1" ]; then				\
	  cp $@ $*.expected;						\
	fi;								\
	if test -e $*.expected; then					\
	  if diff $@ $*.expected >& /dev/null; then			\
	    if $(MENHIR) --explain $$CMD >& $*.out; then		\
	      echo "[OK] $$FILES";					\
	    else							\
	      if ! grep $* 00KNOWN_FAILURES >& /dev/null; then (	\
	        echo "-> [KO] $$FILES:";				\
	        echo "   menhir --only-preprocess succeeded,"; 		\
	        echo "   but menhir failed."; 				\
	        echo "   more $*.out";					\
	      ) | tee -a failures; 					\
	      else                                                      \
	        echo "-> [KO] $$FILES:";				\
	        echo "   known failure.";				\
	      fi;							\
	    fi;								\
	  else (							\
	    echo "-> [KO] $$FILES:";					\
	    echo "   menhir --only-preprocess produced wrong output."; 	\
	    echo "   diff $*.expected $@";				\
	  ) | tee -a failures;						\
	  fi;								\
	else								\
	  echo "Warning: missing file: $*.expected" | tee -a warnings; 	\
	fi;								\

expected:
	@ echo "Are you sure ? (this will erase *.expected) [Press ENTER]"
	@ read
	@ mkdir -p /tmp/menhir-expected
	@ cp *.expected /tmp/menhir-expected >& /dev/null || true
	@ CREATE_EXPECTED=1 $(MAKE) -s test
	@ echo "Expected output re-generated."

clean:
	rm -f *.ml *.mli *.conflicts *.automaton *.cmi *.cmo *.cmx *.o *.s *.result *~ failures warnings

