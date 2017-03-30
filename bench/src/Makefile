OCAMLBUILD := ocamlbuild -use-ocamlfind -classic-display
TARGET     := test.byte

.PHONY: all test expected clean

all:
	$(OCAMLBUILD) $(TARGET)

test: all
	$(MAKE) -C ../../src bootstrap
	./$(TARGET) --verbosity 1

expected: all
	$(MAKE) -C ../../src bootstrap
	./$(TARGET) --verbosity 1 --create-expected

clean:
	rm -f *~ *.native *.byte
	$(OCAMLBUILD) -clean
