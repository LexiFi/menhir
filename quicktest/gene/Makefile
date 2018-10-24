.PHONY: all clean realclean

# Build an absolute path for Menhir.
# This is useful because ocamlbuild descends into _build.
ifndef MENHIR
  READLINK := $(shell if which greadlink >/dev/null ; then echo greadlink ; else echo readlink ; fi)
  MENHIR   := $(shell $(READLINK) -f ../../src/_stage1/menhir.native)
endif
ifndef OCAMLBUILD
  OCAMLBUILD := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR)" $(TAGS)
endif

all:
	$(OCAMLBUILD) gene.native

clean:
	$(OCAMLBUILD) -clean

realclean:
	rm -f gene.{code,table,ocamlyacc}
	rm -f *.time *.out
