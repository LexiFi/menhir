# -------------------------------------------------------------------------

# This Makefile helps perform tests and prepare releases. This is *not* the
# Makefile that compiles and installs Menhir on a user's machine.

# Require bash.
SHELL := bash
# Prevent the built-in bash cd from displaying information.
export CDPATH=

# -------------------------------------------------------------------------

# A dummy entry.

.PHONY: all
all:
	@echo Please go down into src/ if you wish to compile Menhir.

# -------------------------------------------------------------------------

# Testing.

# This assumes that [make -C src everyday bootstrap] has been run
# (or that MENHIR is set and points to a Menhir executable that
# one wishes to test).

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

HEADACHE        := headache
SRCHEAD         := $(CURRENT)/headers/regular-header
LIBHEAD         := $(CURRENT)/headers/library-header
COQLIBHEAD      := $(CURRENT)/headers/coq-library-header
HEADACHECOQCONF := $(CURRENT)/headers/headache-coq.conf
FIND            := $(shell if command -v gfind >/dev/null ; then echo gfind ; else echo find ; fi)

.PHONY: headache
headache:
	@ cd src && $(FIND) . -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(SRCHEAD) "{}" ";"
	@ for file in src/standard.mly $(MENHIRLIB_FILES) ; do \
	    $(HEADACHE) -h $(LIBHEAD) $$file ; \
	  done
	@ for file in coq-menhirlib/src/*.v ; do \
	    $(HEADACHE) -h $(COQLIBHEAD) -c $(HEADACHECOQCONF) $$file ; \
	  done

# -------------------------------------------------------------------------

# Creating a release.

# A release commit is created off the main branch, on the side, and tagged.
# Indeed, some files need to be changed or removed for a release.

BRANCH := release-branch-$(DATE)

# The documentation files $(DOC) are copied to the directory $(RELEASE) on the
# master branch. They are also copied to the directory $(WWW).

DOC     := doc/manual.pdf doc/manual.html doc/manual*.png
RELEASE := releases/$(DATE)
WWW     := www

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
# Check the current package description.
	@ opam lint
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
	@ make --quiet -C coq-menhirlib clean
	@ git rm -rf attic headers quicktest releases src/attic test --quiet
# Remove files that do not need to (or must not) be distributed.
# Keep check-tarball.sh because it is used below.
	@ git rm GNUmakefile HOWTO.md TODO* *.opam coq-menhirlib/descr --quiet
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
	@ echo 'Definition require_$(DATE).' >> coq-menhirlib/src/Version.v
	@ git add coq-menhirlib/src/Version.v
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
	@ cd $(RELEASE) && git add -f $(DOC)
	@ echo "Publishing the documentation online..."
	@ cd $(WWW) && git rm -rf doc
	@ cd $(WWW) && cp -r ../$(RELEASE)/doc .
	@ cd $(WWW) && git add $(DOC)
	@ git commit -m "Saved and published documentation for release $(DATE)."
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  \"make publish\"   to push this release to gitlab.inria.fr"
	@ echo "  \"make export\"    to upload the manual to yquem.inria.fr"
	@ echo "  \"make opam\"      to create a new opam package"
	@ echo "Otherwise, please type:"
	@ echo "  \"make undo\"      to undo this release"

.PHONY: publish
publish:
# Push the new branch and tag to gitlab.inria.fr.
	@ git push origin $(BRANCH)
	@ git push --tags

.PHONY: undo
undo:
# Delete the new branch and tag.
	@ git branch -D $(BRANCH)
	@ git tag -d $(DATE)
# Delete the two new commits on the master branch.
	@ git reset --hard HEAD~1

# -------------------------------------------------------------------------

# Copying the documentation to François' page on yquem.

# I would have like to serve these files on gitlab.inria.fr,
# but I don't know how to make them look like native .html
# and .pdf files.
# Also, I don't know how to obtain a stable URL that always
# points to the latest released version of the documentation.

RSYNC   := scp -p -C
TARGET  := yquem.inria.fr:public_html/menhir/
PAGE    := /home/fpottier/dev/page

# This assumes that [make release] has been run.

.PHONY: export
export:
# Copy the documentation to yquem.
	$(RSYNC) $(RELEASE)/doc/* $(TARGET)

# -------------------------------------------------------------------------

# Publishing a new version of the opam packages.

# This entry assumes that [make release] has been run on the same day.

# There are two opam packages: one for menhir (part of the OCaml opam
# repository) and one for coq-menhirlib (part of the Coq opam repository).

# You need a version of opam-publish that supports --packages:
#   git clone git@github.com:fpottier/opam-publish.git
#   cd opam-publish
#   git checkout 2.0
#   opam pin add opam-publish.dev .

# The following command should have been run once:
#   opam publish repo add opam-coq-archive coq/opam-coq-archive

# The package name.
THIS     := menhir
THAT     := coq-menhirlib

# Menhir's repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/$(THIS)

# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz

# Additional options for coq-menhirlib.
COQ_MENHIRLIB_PUBLISH_OPTIONS := \
  --repo opam-coq-archive \
  --packages packages/released \

.PHONY: opam
opam:
# Publish an opam description for menhir.
	@ opam publish -v $(DATE) $(THIS).opam $(ARCHIVE)
# Patch coq-menhirlib.opam to add a strong dependency on Menhir
# with the exact same version number.
	@ cp $(THAT).opam $(THAT).patched.opam
	@ sed -i 's/"menhir" { = "dev" }/"menhir" { = "$(DATE)" }/g' $(THAT).patched.opam
# Publish an opam description for coq-menhirlib.
	@ opam publish -v $(DATE) $(COQ_MENHIRLIB_PUBLISH_OPTIONS) $(THAT).patched.opam $(ARCHIVE)
	@ rm $(THAT).patched.opam

# -------------------------------------------------------------------------

# Re-installing locally. This can overwrite an existing local installation.

.PHONY: pin
pin:
	opam pin add menhir.dev .

.PHONY: unpin
unpin:
	opam pin remove menhir

.PHONY: reinstall
reinstall:
	opam reinstall -v --working-dir menhir
# We do not use --assume-built,
# as it would require first re-building everything using
#   make PREFIX=`pwd`/src -f Makefile all
# and that is time-consuming.

# -------------------------------------------------------------------------

# Running the Markdown linter on our Markdown files.

# For an explanation of mdl's error messages, see:
# https://github.com/mivok/markdownlint/blob/master/docs/RULES.md

MDFILES := *.md */*.md

.PHONY: mdl
mdl:
	@ for f in $(MDFILES) ; do \
	  cp $$f $$f.bak && expand $$f.bak > $$f && rm $$f.bak ; \
	done
	@ mdl $(MDFILES)
