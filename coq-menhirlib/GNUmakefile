# -------------------------------------------------------------------------

# Private Makefile for package maintenance.

SHELL := bash
export CDPATH=

.PHONY: package export opam submit pin unpin

# -------------------------------------------------------------------------

include Makefile

# -------------------------------------------------------------------------

# Distribution.

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The project name on gitlab.
PROJECT  := coq-menhirlib
# The opam package name.
PACKAGE  := coq-menhirlib
# The repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/$(PROJECT)
# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz
# The local repository directory.
PWD      := $(shell pwd)

# -------------------------------------------------------------------------

# Prepare a release.

package:
# Make sure the correct version can be installed.
	@ make uninstall
	@ make clean
	@ make install

# -------------------------------------------------------------------------

# Publish a release. (Remember to commit everything first!)

export:
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Upload. (This automatically makes a .tar.gz archive available on gitlab.)
	@ git push
	@ git push --tags

# -------------------------------------------------------------------------

# Updating the opam package.

# This entry assumes that "make package" and "make export"
# have just been run (on the same day).

# You need opam-publish:
#   sudo apt-get install libssl-dev
#   opam install tls opam-publish

# In fact, you need a version of opam-publish that supports --subdirectory:
#   git clone git@github.com:fpottier/opam-publish.git
#   cd opam-publish
#   git checkout 1.3
#   opam pin add opam-publish `pwd` -k git

# The following command should have been run once:
#   opam-publish repo add opam-coq-archive coq/opam-coq-archive

PUBLISH_OPTIONS := \
  --repo opam-coq-archive \
  --subdirectory released \

opam:
	@ opam lint
	@ opam-publish prepare $(PUBLISH_OPTIONS) $(PACKAGE).$(DATE) $(ARCHIVE)

submit:
	@ opam-publish submit $(PUBLISH_OPTIONS) $(PACKAGE).$(DATE)

# -------------------------------------------------------------------------

# Pinning.

pin:
	opam pin add $(PACKAGE) `pwd` -k git

unpin:
	opam pin remove $(PACKAGE)
