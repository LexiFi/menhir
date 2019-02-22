PWD := $(shell pwd)

COQINCLUDE := -R $(PWD) MenhirLib \

export

.PHONY: all clean install uninstall

_CoqProject:
	@ $(MAKE) -f Makefile.coq --no-print-directory _CoqProject

all: _CoqProject
	@ $(MAKE) -f Makefile.coq --no-print-directory all

clean:
	@ $(MAKE) -f Makefile.coq --no-print-directory clean
	@ rm -f _CoqProject

CONTRIB := $(shell $(COQBIN)coqc -where)/user-contrib
TARGET  := $(CONTRIB)/MenhirLib

install: all
	rm -rf $(TARGET)
	mkdir -p $(TARGET)
	install -m 644 *.v *.vo *.glob $(TARGET)

uninstall:
	rm -rf $(TARGET)
