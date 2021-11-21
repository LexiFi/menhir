include ../Makefile.demo

# Choosing a PDF viewer.
VIEW := $(shell \
  if command -v open >/dev/null ; then echo open ; else echo evince ; fi)

# Viewing a .pdf image of the LR(1) automaton.
.PHONY: view
view: all
	$(VIEW) $(IMAGE)/parser.pdf
