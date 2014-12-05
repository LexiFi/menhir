.PHONY: all clean

# Build an absolute path for Menhir.
# This is useful because ocamlbuild descends into _build.
ifndef MENHIR
  READLINK := $(shell if which greadlink >/dev/null ; then echo greadlink ; else echo readlink ; fi)
  MENHIR   := $(shell $(READLINK) -f ../../src/_stage1/menhir.native) \
	      --table
endif
OCAMLBUILD := ocamlbuild -use-ocamlfind -menhir "$(MENHIR)"

all:
	$(OCAMLBUILD) gene.native

clean:
	$(OCAMLBUILD) -clean
