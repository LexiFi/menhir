.PHONY: all test clean

# The Menhir executable in the PATH and the library MenhirSdk
# should agree on their version number, or this test will fail
# complaining that magic strings do not match.
MENHIR     := menhir
DUNE       := jbuilder
MAIN       := generate
EXECUTABLE := menhir-generate-printers

all:
	$(DUNE) build $(MAIN).exe
	cp -f _build/default/$(MAIN).exe $(EXECUTABLE)

TEST       := ../../test/good/parser_raw

test: all
	rm -f $(TEST).cmly
	$(MENHIR) --cmly $(TEST).mly
	./$(EXECUTABLE) $(TEST).cmly

clean:
	rm -f *~ .*~
	$(DUNE) clean
	rm -f $(EXECUTABLE)
