.PHONY: clean test expected list

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
TESTS=$(shell ls *.mly | egrep '.*([1]|[^2-9]).mly')
RESULTS=$(TESTS:.mly=.opp.out)

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

# The file %.flags    (if it exists) stores extra flags that should be passed to Menhir.

# The file %.opp.out  stores the output of menhir --only-preprocess.
# The file %.opp.exp  stores its expected output.
# The file %.out      stores the output of menhir.
# The file %.exp      stores its expected output.

%.opp.out: %.mly
	@ if echo $< | grep -e "-1.mly" &> /dev/null; then		\
	  export BASE=`echo $< | sed s/"-1.mly"/""/`;			\
	  export FILES=`echo $$BASE-[1-9].mly`;				\
	  export CMD="$$FILES --base $$BASE";				\
	else								\
	  export FILES="$<";						\
	  export BASE=`echo $< | sed s/".mly"/""/`;			\
	  export CMD="$<";						\
	fi;								\
	if [ -f $*.flags ] ; then                                       \
	  export CMD="$$CMD `cat $*.flags`" ;                            \
	fi;                                                             \
	$(SHELL) -c '$(MENHIR) --only-preprocess $$CMD >& $@';		\
	if [ x$$CREATE_EXPECTED == x"1" ]; then				\
	  cp $@ $*.opp.exp;						\
	fi;								\
	if test -e $*.opp.exp; then					\
	  if diff $@ $*.opp.exp >& /dev/null; then			\
	    if $(MENHIR) --explain $$CMD >& $*.out; then		\
	      if [ x$$CREATE_EXPECTED == x"1" ]; then			\
		cp $*.out $*.exp;					\
	      fi;							\
	      if test -e $*.exp; then					\
	        if ! diff $*.exp $*.out >& /dev/null; then (		\
		  echo "-> [KO] $$FILES:";				\
		  echo "   menhir produced wrong output.";		\
	          echo "   diff $*.exp $*.out";				\
	        ) | tee -a failures;					\
	        else							\
	          echo "[OK] $$FILES";					\
	        fi;							\
	      else							\
	        echo "Warning: missing file: $*.exp" | tee -a warnings; \
	      fi;							\
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
	    echo "   diff $*.opp.exp $@";				\
	  ) | tee -a failures;						\
	  fi;								\
	else								\
	  echo "Warning: missing file: $*.opp.exp" | tee -a warnings; 	\
	fi;								\

expected:
	@ echo "Are you sure ? (this will erase *.exp) [Press ENTER]"
	@ read
	@ mkdir -p /tmp/menhir-expected
	@ cp *.exp /tmp/menhir-expected >& /dev/null || true
	@ CREATE_EXPECTED=1 $(MAKE) -s test
	@ echo "Expected output re-generated."

clean::
	rm -f *~
	rm -f *.ml *.mli *.conflicts *.automaton
	rm -f *.out failures warnings

# ------------------------------------------------------------------------------
# Testing LRijkstra, that is, menhir --list-errors.

ifeq ($(shell hostname),teraram)
  TIMEOUT  := 7200
  PARALLEL := -j40
else
  TIMEOUT  := 2
  PARALLEL := -j4
endif

SINGLE    = $(shell ls *.mly | egrep -v '.*-([1-9]).mly')

%.log: %.mly
	@ echo "Now dealing with: $<" | tee -a $@
	@ if [ -f $*.flags ] ; then \
	  FLAGS=`cat $*.flags` ; \
	fi; \
	if (timeout $(TIMEOUT) $(MENHIR) $$FLAGS --list-errors -la 2 --lalr $<) >>$@ 2>&1 ; then \
	    echo "$<: success." | tee -a $@ ; \
          else \
	    echo "$<: TIMEOUT (or failure)." | tee -a $@ ; \
	  fi

list:
# Compile Menhir.
#	make -C $(SRC) clean
	make -C $(SRC) bootstrap
# Remove any leftover output files.
	rm -f *.log lr.csv
# Print the header of lr.csv. (This should be kept in sync with LRijkstra.ml.)
	echo "grammar,terminals,nonterminals,size,states,trie,facts,edgefacts,time,heap" > lr.csv
# Try every grammar. (Only the single-file grammars, that is.)
# This can proceed in parallel, for a reasonable number of processes.
# Each process appends one line of data to lr.csv when it finishes.
# Hopefully the final content of lr.csv will be a reasonable interleaving
# of these lines.
	@ time $(MAKE) $(PARALLEL) $(patsubst %.mly,%.log,$(SINGLE))
# Finished.
	@ echo "Number of grammars that could not be handled in $(TIMEOUT) seconds:"
	@ grep TIMEOUT *.log | wc -l
	@ echo "Number of grammars that were successfully handled:"
	@ tail -n +2 lr.csv  | wc -l

clean::
	rm -f *.log
#	rm -f lr.csv

