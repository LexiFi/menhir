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

# By default, we attempt to use ocamlfind (if present in the PATH), but it
# is possible to prevent that externally by setting USE_OCAMLFIND to false.

ifndef USE_OCAMLFIND
  USE_OCAMLFIND = ocamlfind ocamlc -v >/dev/null 2>&1
endif

# ----------------------------------------------------------------------------
# Installation paths.
# These may be overridden from outside; e.g., our opam package description
# provides its own values of docdir, libdir, and mandir.

bindir          := $(PREFIX)/bin
docdir		:= $(PREFIX)/share/doc/menhir
libdir	        := $(PREFIX)/share/menhir
mandir          := $(PREFIX)/share/man/man1
MANS            := menhir.1
DOCS            := manual.pdf demos
MLYLIB          := src/standard.mly

# ----------------------------------------------------------------------------

# A few settings differ on Windows versus Unix.

# If the compiler is MSVC, then the name of the executable file ends in .exe,
# and object file names end in .obj instead of .o.

ifeq "$(shell ocamlc -config | grep ccomp_type)" "ccomp_type: msvc"
  MENHIREXE    := menhir.exe
  OBJ          := obj
# LIBSUFFIX    := lib
else
  MENHIREXE    := menhir
  OBJ          := o
# LIBSUFFIX    := a
endif

# The path $(installation_libdir), which is recorded in src/installation.ml (see
# below), must sometimes be translated using cygpath.

# This one is tricky. To summarize, if I understood correctly, we can assume
# that Cygwin always exists when Menhir is compiled and installed (because
# executing a Makefile, like this one, requires Cygwin), but we cannot assume
# that Menhir will be executed under Cygwin. If the OCaml compiler is
# configured to produce a Cygwin executable, then, yes, Cygwin is there at
# execution time, so path translation is not necessary (and should not be
# performed). On the other hand, if the OCaml compiler is configured to
# produce a native Windows executable, then Cygwin is not there at execution
# time and path translation is required. In summary, path translation must be
# performed if "os_type" is "Win32" or "Win64", and must not be performed if
# "os_type" is "Cygwin" or "Unix".

ifneq (,$(findstring "os_type: Win", "$(shell ocamlc -config | grep os_type)"))
installation_libdir := $(shell cygpath -m $(libdir))
else
installation_libdir := $(libdir)
endif

# ----------------------------------------------------------------------------
# Compilation.

# The directory where things are built.
BUILDDIR := src/_stage2

# Installation time settings are recorded within src/installation.ml.
# This file is recreated every time so as to avoid becoming stale.

.PHONY: all install uninstall

all:
	@ rm -f src/installation.ml
	@ echo "let libdir = \"$(installation_libdir)\"" > src/installation.ml
	@ if $(USE_OCAMLFIND) ; then \
	  echo "let ocamlfind = true" >> src/installation.ml ; \
	else \
	  echo "let ocamlfind = false" >> src/installation.ml ; \
	fi
	@ $(MAKE) -C src library bootstrap
	@ $(MAKE) $(BUILDDIR)/menhirLib.ml $(BUILDDIR)/menhirLib.mli

# -------------------------------------------------------------------------

# The names of the modules in MenhirLib are obtained by reading the
# non-comment lines in menhirLib.mlpack.

MENHIRLIB_MODULES := $(shell grep -ve "^[ \t\n\r]*\#" src/menhirLib.mlpack)

# The source file menhirLib.ml is created by concatenating all of the source
# files that make up MenhirLib. This file is not needed to compile Menhir or
# MenhirLib. It is installed at the same time as MenhirLib and is copied by
# Menhir when the user requests a self-contained parser (one that is not
# dependent on MenhirLib).

$(BUILDDIR)/menhirLib.ml:
	@ rm -f $@
	@ for m in $(MENHIRLIB_MODULES) ; do \
	  echo "module $$m = struct" >> $@ ; \
	  cat src/$$m.ml >> $@ ; \
	  echo "end" >> $@ ; \
	done

# The source file menhirLib.mli is created in the same way. If a module
# does not have an .mli file, then we assume that its .ml file contains
# type (and module type) definitions only, so we copy it instead of the
# (non-existent) .mli file.

$(BUILDDIR)/menhirLib.mli:
	@ rm -f $@
	@ for m in $(MENHIRLIB_MODULES) ; do \
	  echo "module $$m : sig" >> $@ ; \
	  if [ -f src/$$m.mli ] ; then \
	    cat src/$$m.mli >> $@ ; \
	  else \
	    cat src/$$m.ml >> $@ ; \
	  fi ; \
	  echo "end" >> $@ ; \
	done

# -------------------------------------------------------------------------

# The files that should be installed as part of menhirLib.

MENHIRLIB       := menhirLib.mli menhirLib.ml menhirLib.cmi menhirLib.cmo
ifneq ($(TARGET),byte)
MENHIRLIB       := $(MENHIRLIB) menhirLib.cmx menhirLib.$(OBJ)
endif

# ----------------------------------------------------------------------------
# Installation.

install:
	mkdir -p $(bindir)
	mkdir -p $(libdir)
	mkdir -p $(docdir)
	mkdir -p $(mandir)
	install $(BUILDDIR)/menhir.$(TARGET) $(bindir)/$(MENHIREXE)
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
