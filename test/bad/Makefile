.PHONY: test clean expected

# ------------------------------------------------------------------------------
# The test scripts are now written in OCaml.

test expected:
	@ $(MAKE) -C ../src $@

clean::
	rm -f *~ *.ml *.mli *.conflicts *.automaton
	rm -f *.result
