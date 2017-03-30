OCAMLBUILD := ocamlbuild -use-ocamlfind -classic-display
TARGET     := test.byte

.PHONY: all test clean

all:
	$(OCAMLBUILD) $(TARGET)

test: all
	$(MAKE) -C ../../src bootstrap
	./$(TARGET)

clean:
	rm -f *~ *.native *.byte
	$(OCAMLBUILD) -clean
