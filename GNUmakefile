# -------------------------------------------------------------------------

# The main purpose of this Makefile is to help perform tests and
# prepare releases. This is *not* the Makefile that compiles and
# installs Menhir on a user's machine.

SHELL := bash

# -------------------------------------------------------------------------

# A dummy entry.

.PHONY: all
all:
	@echo Please go down into src/ if you wish to compile Menhir.

# -------------------------------------------------------------------------

# Utilities.

MD5SUM := $(shell if command -v md5 2>/dev/null ; then echo "md5 -r" ; else echo md5sum ; fi)

# -------------------------------------------------------------------------

# Testing.
# Assumes that "make bootstrap" has been run in src/
# or that MENHIR is properly set.

.PHONY: test
test:
	$(MAKE) -C test

# -------------------------------------------------------------------------

# Cleaning up.

.PHONY: clean
clean:
	@ for i in test demos src quicktest doc ; do \
	  $(MAKE) -C $$i $@ ; \
	done

# -------------------------------------------------------------------------

# Prevent the built-in bash cd from displaying information.

export CDPATH=

# -------------------------------------------------------------------------

# Distribution.

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

PACKAGE  := menhir-$(DATE)
CURRENT  := $(shell pwd)
TARBALL  := $(CURRENT)/$(PACKAGE).tar.gz

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

# Propagating an appropriate header into every file.

# This requires a version of headache that supports UTF-8; please use
# https://github.com/fpottier/headache

# This used to be done at release time and not in the repository, but
# it is preferable to do in it the repository too, for two reasons: 1-
# the repository is (or will be) publicly accessible; and 2- this makes
# it easier to understand the line numbers that we sometimes receive as
# part of bug reports.

# Menhir's standard library (standard.mly) as well as the source files
# in MenhirLib carry the "library" license, while every other file
# carries the "regular" license.

HEADACHE := headache
SRCHEAD  := $(CURRENT)/headers/regular-header
LIBHEAD  := $(CURRENT)/headers/library-header
FIND     := $(shell if command -v gfind >/dev/null ; then echo gfind ; else echo find ; fi)

.PHONY: headache
headache:
	@ cd src && $(FIND) . -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(SRCHEAD) "{}" ";"
	@ for file in src/standard.mly $(MENHIRLIB_FILES) ; do \
	    $(HEADACHE) -h $(LIBHEAD) $$file ; \
	  done

# -------------------------------------------------------------------------

# Creating a release.

# A release commit is created off the main branch, on the side, and tagged.
# Indeed, some files need to be changed or removed for a release.

BRANCH := release-branch-$(DATE)

# The documentation files $(DOC) are copied to the directory $(LOG) on the
# master branch, for the record. This allows us to easily access all of the
# documentation for earlier versions of Menhir.

DOC     := doc/manual.pdf doc/manual.html doc/manual*.png
RELEASE := releases/$(DATE)

# Prior to making a release, one should run [make test],
# then [make pin] and [make -C demos].

.PHONY: release
release:
# Check if this is the master branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "master" ] ; then \
	  echo "Error: this is not the master branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  fi
# Create a fresh git branch and switch to it.
	@ echo "Preparing a release commit on a fresh release branch..."
	@ git checkout -b $(BRANCH)
# In src/_tags, remove every line tagged "my_warnings".
	@ cd src && grep -v my_warnings _tags > _tags.new && mv _tags.new _tags
	@ git add src/_tags
# The file src/installation.ml is not under version control, so won't be
# included in the archive. We nevertheless remove it, for a clean test
# build below.
	@ rm -f src/installation.ml
# Remove subdirectories that do not need to (or must not) be distributed.
	@ make --quiet -C test clean
	@ make --quiet -C quicktest clean
	@ git rm -rf attic headers quicktest releases src/attic test --quiet
# Remove files that do not need to (or must not) be distributed.
# Keep check-tarball.sh because it is used below.
	@ git rm GNUmakefile HOWTO.md TODO* opam --quiet
# Hardcode the version number in the files that mention it. These
# include version.ml, StaticVersion.{ml,mli}, version.tex, META.
	@ echo let version = \"$(DATE)\" > src/version.ml
	@ git add src/version.ml
	@ echo version = \"$(DATE)\" >> src/menhirLib.META
	@ echo version = \"$(DATE)\" >> src/menhirSdk.META
	@ git add src/menhirLib.META src/menhirSdk.META
	@ echo "let require_$(DATE) = ()" > src/StaticVersion.ml
	@ echo "val require_$(DATE) : unit" > src/StaticVersion.mli
	@ git add src/StaticVersion.ml src/StaticVersion.mli
	@ echo '\gdef\menhirversion{$(DATE)}' > doc/version.tex
	@ git add doc/version.tex
# Compile the documentation.
	@ echo "Building the documentation..."
	@ make --quiet -C doc clean >/dev/null
	@ make --quiet -C doc all   >/dev/null
	@ git add -f $(DOC)
# Commit.
	@ echo "Committing..."
	@ git commit -m "Release $(DATE)." --quiet
# Check that the build and installation seem to work.
# We build our own archive, which is not necessarily identical to the one
# that gitlab creates for us once we publish our release. This should be
# good enough.
	@ echo "Creating an archive..."
	@ git archive --prefix=$(PACKAGE)/ --format=tar.gz --output=$(TARBALL) HEAD
	@ echo "Checking that this archive can be compiled and installed..."
	@ ./check-tarball.sh $(PACKAGE)
	@ echo "Removing this archive..."
	@ rm $(TARBALL)
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Save a copy of the manual.
	@ mkdir -p $(RELEASE)/doc
	@ cp $(DOC) $(RELEASE)/doc
# Switch back to the master branch.
	@ echo "Switching back to the master branch..."
	@ git checkout master
# Commit a copy of the manual *in the master branch* in releases/.
	@ echo "Committing a copy of the documentation..."
	@ cd $(RELEASE)/doc && git add -f *
	@ git commit -m "Saved documentation for release $(DATE)."
# Set a current release pointer which allows us to have a stable URL for
# the documentation of the latest released version.
	@ ln -sf $(RELEASE) ./current
	@ git add current
	@ git commit -m "Set symbolic link to current release."
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  git push origin $(BRANCH) && git push --tags"
	@ echo "If unhappy, please type:"
	@ echo "  git branch -D $(BRANCH) && git tag -d $(DATE) && git reset --hard HEAD~2"

# -------------------------------------------------------------------------

# Updating the opam package.

# TEMPORARY out of date

# This entry assumes that "make package" and "make export" have been
# run on the same day.

OPAM := $(HOME)/dev/opam-repository
CSUM  = $(shell $(MD5SUM) menhir-$(DATE).tar.gz | cut -d ' ' -f 1)

.PHONY: opam
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
# TEMPORARY:
# THIS NO LONGER WORKS; THE URL AND CHECKSUM ARE NOW STORED IN opam.
	@ cd $(OPAM)/packages/menhir/menhir.$(DATE) && \
	  rm url && \
	  echo 'archive: "http://gallium.inria.fr/~fpottier/menhir/menhir-$(DATE).tar.gz"' >> url && \
	  echo 'checksum: "$(CSUM)"' >> url
# Copy the file "opam" from Menhir's repository to opam's.
	@ cp -f opam $(OPAM)/packages/menhir/menhir.$(DATE)
# Prepare a commit.
	@ echo "Preparing a new commit..."
	@ cd $(OPAM)/packages/menhir && \
	  git add menhir.$(DATE) && \
	  git status
# Ask for review.
	@ echo "If happy, please run:"
	@ echo "  cd $(OPAM)/packages/menhir && git commit -a && git push && firefox https://github.com/fpottier/opam-repository.git"
	@ echo "and issue a pull request."

# -------------------------------------------------------------------------

# Re-installing locally. This can overwrite an existing local installation.

.PHONY: pin
pin:
	opam pin add menhir .

.PHONY: unpin
unpin:
	opam pin remove menhir

# -------------------------------------------------------------------------

# Running the Markdown linter on our Markdown files.

# For an explanation of mdl's error messages, see:
# https://github.com/mivok/markdownlint/blob/master/docs/RULES.md

.PHONY: mdl
mdl:
	@ for f in *.md ; do \
	  cp $$f $$f.bak && expand $$f.bak > $$f && rm $$f.bak ; \
	done
	@ mdl *.md */*.md
