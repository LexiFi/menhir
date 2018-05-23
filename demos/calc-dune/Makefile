.PHONY: all clean test

DUNE       := jbuilder
EXECUTABLE := calc.exe

all:
	$(DUNE) build $(EXECUTABLE)

clean:
	$(DUNE) clean
	rm -f *~ .*~

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ./$(EXECUTABLE)
