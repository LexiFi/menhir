# This is the main Makefile that is shipped as part of the source package.
# Keep in mind that the hierarchy that is shipped is not identical to the
# hierarchy within the svn repository: some sub-directories are not shipped;
# the documentation is pre-built.

# The hierarchy that is shipped includes:
#   demos
#   menhir.1
#   manual.pdf
#   src
#   Makefile (this one)

# ----------------------------------------------------------------------------

# The following variables must/can be configured.

ifndef PREFIX
  $(error Please define PREFIX)
endif

ifndef TARGET
  TARGET := native
endif

# ----------------------------------------------------------------------------

# By default, we attempt to use ocamlfind (if present in the PATH), but it it
# is possible to prevent that externally by setting USE_OCAMLFIND to false.

ifndef USE_OCAMLFIND
  USE_OCAMLFIND = ocamlfind ocamlc -v >/dev/null 2>&1
endif

# ----------------------------------------------------------------------------

# A few settings differ on Windows versus Unix.

ifeq "$(shell ocamlc -config | grep ccomp_type)" "ccomp_type: msvc"
MENHIREXE    := menhir.exe
OBJ          := obj
# LIBSUFFIX    := lib
else
MENHIREXE    := menhir
OBJ          := o
# LIBSUFFIX    := a
endif

# ----------------------------------------------------------------------------
# Installation paths.

bindir          := ${PREFIX}/bin
docdir		:= ${PREFIX}/share/doc/menhir
libdir	        := ${PREFIX}/share/menhir
mandir          := ${PREFIX}/share/man/man1
MANS            := menhir.1
DOCS            := manual.pdf demos
MLYLIB          := src/standard.mly

# -------------------------------------------------------------------------

# Building menhirLib.

ifeq ($(TARGET),byte)
MENHIRLIB       := menhirLib.cmi menhirLib.cmo
else
MENHIRLIB       := menhirLib.cmi menhirLib.cmo menhirLib.cmx menhirLib.$(OBJ)
endif

# ----------------------------------------------------------------------------
# Compilation.

# Installation time settings are recorded within src/installation.ml.
# This file is recreated every time so as to avoid becoming stale.

.PHONY: all install uninstall

all:
	rm -f src/installation.ml
	echo "let libdir = \"${libdir}\"" > src/installation.ml
	if $(USE_OCAMLFIND) ; then \
	  echo "let ocamlfind = true" >> src/installation.ml ; \
	else \
	  echo "let ocamlfind = false" >> src/installation.ml ; \
	fi
	$(MAKE) -C src library bootstrap

# ----------------------------------------------------------------------------
# Installation.

# The directory where things have been built (by make all, above).
BUILDDIR := src/_stage2

install: all
	mkdir -p $(bindir)
	mkdir -p $(libdir)
	mkdir -p $(docdir)
	mkdir -p $(mandir)
	install $(BUILDDIR)/menhir.native $(bindir)/$(MENHIREXE)
	install -m 644 $(MLYLIB) $(libdir)
	cp -r $(DOCS) $(docdir)
	cp -r $(MANS) $(mandir)
	@if $(USE_OCAMLFIND) ; then \
	  echo Installing MenhirLib via ocamlfind. ; \
	  ocamlfind install menhirLib src/META $(patsubst %,$(BUILDDIR)/%,$(MENHIRLIB)) ; \
	else \
	  echo Installing MenhirLib manually. ; \
	  install -m 644 $(patsubst %,$(BUILDDIR)/%,$(MENHIRLIB)) $(libdir) ; \
	fi

uninstall:
	rm -rf $(bindir)/$(MENHIREXE)
	rm -rf $(libdir)
	rm -rf $(docdir)
	rm -rf $(mandir)/$(MANS)
	@if $(USE_OCAMLFIND) ; then \
	  echo Un-installing MenhirLib via ocamlfind. ; \
	  ocamlfind remove menhirLib ; \
	fi
