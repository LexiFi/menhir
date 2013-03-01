# This is a development Makefile, which is not shipped.

# Prevent an immediate attempt to build .depend when "make bootstrap"
# is run.

COLD += bootstrap api

# Option 1: incremental development. This assumes that a ./menhir
# executable already exists. (If that is not the case, bootstrap
# first.) To use it, we set a few variables, include the user
# Makefile, and build menhir.{byte,opt}.

all: menhir.byte menhir.opt

PARSER       := fancy-parser
MENHIR	     := ./menhir
PGEN          = $(shell ([ -f $(MENHIR) ] && echo $(MENHIR) $(PGFLAGS)) \
	          || $(error "$(MENHIR) not found. \
                              Please bootstrap first (make bootstrap)."))

-include Makefile

OFLAGS       += -dtypes

# Option 2: bootstrapping. This must be used when the src/ directory
# is clean.  After creating installation.ml, we just run the normal
# bootstrapping process, as described by the user Makefile.

.PHONY: bootstrap

bootstrap: installation.ml
	$(MAKE) -f Makefile

# When developing, the standard library path is the current directory.
# ocamlfind is not assumed.

installation.ml:
	echo "let libdir = \""`pwd`"/\"" > $@
	echo "let ocamlfind = false" >> $@

# Pretty-printing the MenhirLib.Convert API.

api: convert.mli.html

%.mli.html: %.mli
	caml2html -nf $<

clean::
	rm -f *.{ml,mli}.html

