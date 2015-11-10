# -------------------------------------------------------------------------

# This is the development Makefile. It is used for compiling
# development versions and for creating the distributed package.
# This Makefile is not distributed.

SHELL := bash

.PHONY: all test clean package check api export godi opam local unlocal

# -------------------------------------------------------------------------

# A dummy entry.

all:
	@echo Please go down into src/ if you wish to compile Menhir.

# -------------------------------------------------------------------------

# Utilities.

SED     := $(shell if hash gsed 2>/dev/null ; then echo gsed ; else echo sed ; fi)
CUT     := $(shell if hash gcut 2>/dev/null ; then echo gcut ; else echo cut ; fi)
MD5SUM  := $(shell if hash md5  2>/dev/null ; then echo "md5 -r" ; else echo md5sum ; fi)

# -------------------------------------------------------------------------

# Testing.
# Assumes that "make bootstrap" has been run in src/
# or that MENHIR is properly set.

test:
	$(MAKE) -C bench

# -------------------------------------------------------------------------

# Cleaning up.

clean:
	@ for i in bench demos src ; do \
	  $(MAKE) -C $$i $@ ; \
	done
	@ $(MAKE) -rs -C doc $@

# -------------------------------------------------------------------------

# Prevent the built-in bash cd from displaying information.

export CDPATH=

# -------------------------------------------------------------------------

# Distribution.
# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
ifndef $(DATE)
DATE     := $(shell /bin/date +%Y%m%d)
endif

PACKAGE  := menhir-$(DATE)
CURRENT  := $(shell pwd)
TARBALL  := $(CURRENT)/$(PACKAGE).tar.gz
HEADACHE := headache
SRCHEAD  := $(CURRENT)/header
LIBHEAD  := $(CURRENT)/lgpl-header

# -------------------------------------------------------------------------

# A list of files to copy without changes to the package.
#
# This does not include the src/ and doc/ directories, which require
# special treatment.

DISTRIBUTED_FILES := AUTHORS CHANGES INSTALLATION LICENSE Makefile demos

# -------------------------------------------------------------------------

# The names of the modules in MenhirLib are obtained by reading the
# non-comment lines in menhirLib.mlpack.

MENHIRLIB_MODULES := $(shell grep -ve "^[ \t\n\r]*\#" src/menhirLib.mlpack)

# The names of the source files in MenhirLib are obtained by adding
# an .ml or .mli extension to the module name. (We assume that the
# first letter of the file name is a capital letter.)

MENHIRLIB_FILES   := $(shell for m in $(MENHIRLIB_MODULES) ; do \
	                       ls src/$$m.{ml,mli} 2>/dev/null ; \
	                     done)

# -------------------------------------------------------------------------

# Creating a tarball for distribution.

package: clean
# Create a directory to store the distributed files temporarily.
# In src/_tags, every line tagged "my_warnings" is removed.
	@ rm -fr $(PACKAGE)
	@ mkdir -p $(PACKAGE)/src
	@ cp -fr $(DISTRIBUTED_FILES) $(PACKAGE)
	@ cp -fr src/*.ml{,i,y,l,pack} src/*.messages src/Makefile src/META $(PACKAGE)/src
	@ grep -v my_warnings src/_tags > $(PACKAGE)/src/_tags
	@ $(MAKE) -C $(PACKAGE)/demos clean
# Insert headers.
# Note that standard.mly as well as the source files in MenhirLib carry the
# "library" license, while every other file carries the regular "source code"
# license.
	@ echo "-> Inserting headers."
	@ cd $(PACKAGE) && find . -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(SRCHEAD) "{}" ";"
	@ cd $(PACKAGE) && for file in src/standard.mly $(MENHIRLIB_FILES) ; do \
	    $(HEADACHE) -h $(LIBHEAD) $$file ; \
	  done
# Set the version number into the files that mention it. These
# include version.ml, version.tex, META.
	@ echo "-> Setting version to $(DATE)."
	@ echo let version = \"$(DATE)\" > $(PACKAGE)/src/version.ml
	@ echo version = \"$(DATE)\" >> $(PACKAGE)/src/META
# Copy and compile the documentation.
	@ echo "-> Generating the documentation."
	@ cp -r doc $(PACKAGE)
	@ echo '\gdef\menhirversion{$(DATE)}' > $(PACKAGE)/doc/version.tex
	@ make -C $(PACKAGE)/doc clean all
	@ mv $(PACKAGE)/doc/main.pdf $(PACKAGE)/manual.pdf
	@ mv $(PACKAGE)/doc/menhir.1 $(PACKAGE)/
	@ rm -rf $(PACKAGE)/doc
# Create the tarball.
	@ echo "-> Tarball creation."
	tar --exclude=.gitignore -cvz -f $(TARBALL) $(PACKAGE)
	@ echo "-> Package $(PACKAGE).tar.gz is ready."

# -------------------------------------------------------------------------

# Checking the tarball that was created above.

check:
	@ echo "-> Checking the package ..."
# Create a temporary directory; extract, build, and install the
# package into it; build the demos and run the test suite using
# the installed binary.
	@ TEMPDIR=`mktemp -d /tmp/menhir-test.XXXXXX` && { \
	echo "   * Extracting. " && \
	(cd $$TEMPDIR && tar xfz $(TARBALL)) && \
	echo "   * Compiling and installing." && \
	mkdir $$TEMPDIR/install && \
	(cd $$TEMPDIR/$(PACKAGE) \
		&& make PREFIX=$$TEMPDIR/install USE_OCAMLFIND=false all install \
	) > $$TEMPDIR/install.log 2>&1 \
		|| (cat $$TEMPDIR/install.log; exit 1) && \
	echo "   * Building the demos." && \
	(cd $$TEMPDIR/$(PACKAGE) \
		&& $(MAKE) MENHIR=$$TEMPDIR/install/bin/menhir -C demos \
	) > $$TEMPDIR/demos.log 2>&1 \
		|| (cat $$TEMPDIR/demos.log; exit 1) && \
	echo "   * Running the test suite." && \
	$(MAKE) MENHIR=$$TEMPDIR/install/bin/menhir test > $$TEMPDIR/test.log 2>&1 \
		|| (cat $$TEMPDIR/test.log; exit 1) && \
	rm -fr $$TEMPDIR ; }
	@ echo "-> Package $(PACKAGE) seems ready for distribution!"

# -------------------------------------------------------------------------

# Generating HTML documentation for certain API files.

%.mli.html: %.mli
	caml2html -nf $<

%.ml.html: %.ml
	caml2html -nf $<

# -------------------------------------------------------------------------

# Copying the tarball to my Web site.

RSYNC   := scp -p -C
TARGET  := yquem.inria.fr:public_html/menhir/
PAGE    := /home/fpottier/dev/page
API     := src/Convert.mli.html \
	   src/IncrementalEngine.ml.html \
	   src/General.mli.html

api: $(API)

export: api
# Copier l'archive et la doc vers yquem.
	$(RSYNC) $(TARBALL) $(TARGET)
	$(RSYNC) $(PACKAGE)/manual.pdf $(TARGET)
	$(RSYNC) CHANGES $(TARGET)
# Copier l'API vers la page Web.
	$(RSYNC) $(API) $(TARGET)
# Mettre à jour la page Web de Menhir avec le nouveau numéro de version.
	cd $(PAGE) && \
	  $(SED) --in-place=.bak "s/menhir-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/$(PACKAGE)/" menhir.xml && \
	  cvs commit -m "Updated Menhir's version number." && \
	  if [ -x /opt/godi/bin/cduce ] ; then $(MAKE) export ; fi

# -------------------------------------------------------------------------

# Updating the opam package.

# This entry assumes that "make package" and "make export" have been
# run on the same day.

OPAM := $(HOME)/dev/opam-repository
CSUM  = $(shell $(MD5SUM) menhir-$(DATE).tar.gz | cut -d ' ' -f 1)

opam:
# Update my local copy of the opam repository.
	@ echo "Updating local opam repository..."
	@ cd $(OPAM) && \
	  git fetch upstream && \
	  git merge upstream/master
# Create a new Menhir package, based on the last one.
	@ echo "Creating a new package description menhir-$(DATE)..."
	@ cd $(OPAM)/packages/menhir && \
	  cp -r `ls | grep menhir | tail -1` menhir.$(DATE)
# Update the file "url".
	@ cd $(OPAM)/packages/menhir/menhir.$(DATE) && \
	  rm url && \
	  echo 'archive: "http://gallium.inria.fr/~fpottier/menhir/menhir-$(DATE).tar.gz"' >> url && \
	  echo 'checksum: "$(CSUM)"' >> url
# Prepare a commit.
	@ echo "Preparing a new commit..."
	@ cd $(OPAM)/packages/menhir && \
	  git add menhir.$(DATE) && \
	  git status
# Ask for review.
	@ echo "If happy, please run:"
	@ echo "  cd $(OPAM)/packages/menhir && git commit -a && git push && firefox https://github.com/"
	@ echo "and issue a pull request."

# -------------------------------------------------------------------------

# Re-installing locally. This can overwrite an existing local installation.

local:
	$(MAKE) package
	$(MAKE) -C $(PACKAGE) PREFIX=/usr/local USE_OCAMLFIND=true all
	sudo PATH="$(PATH)" $(MAKE) -C $(PACKAGE) PREFIX=/usr/local USE_OCAMLFIND=true install

unlocal:
	sudo PATH="$(PATH)" $(MAKE) -C $(PACKAGE) PREFIX=/usr/local USE_OCAMLFIND=true uninstall

