.PHONY: test expected clean list

SRC=../../src
MENHIR=$(SRC)/_stage2/menhir.native --dump --explain --stdlib $(SRC)

# ------------------------------------------------------------------------------
# The test scripts are now written in OCaml.

test expected:
	@ $(MAKE) -C ../src $@

clean::
	rm -f *~ *.ml *.mli *.conflicts *.automaton
	rm -f *.out

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
