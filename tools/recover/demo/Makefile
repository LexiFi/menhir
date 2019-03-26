.PHONY: all clean test

DUNE       := dune
EXECUTABLE := calc.exe

all:
	@ if command -v $(DUNE) > /dev/null ; then \
	  $(DUNE) build $(EXECUTABLE) ; \
	else \
	  echo "Error: $(DUNE) is required." ; \
	fi

clean:
	dune clean

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ../_build/default/demo/$(EXECUTABLE)
	echo "([1 + 2] * 10) * 2" | ../_build/default/demo/$(EXECUTABLE)
