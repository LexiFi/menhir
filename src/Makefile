.PHONY: everyday bootstrap stage1 stage2 stage3 sdk clean

# ----------------------------------------------------------------------------
# Choose a target.

ifndef TARGET
  TARGET := native
endif

# ----------------------------------------------------------------------------
# Ocamlbuild tool and settings.

OCAMLBUILD := ocamlbuild -classic-display -j 0
ifeq ($(TARGET),byte)
  OCAMLBUILD := $(OCAMLBUILD) -byte-plugin
endif

# ----------------------------------------------------------------------------
# For everyday development.
# Typing "make" will perform just stage 1. This is enough to ensure that
# the source code is correct.

everyday: installation.ml stage1

# ----------------------------------------------------------------------------
# Building Menhir from scratch (a.k.a. bootstrapping).

bootstrap:
	@ $(MAKE) stage1
	@ $(MAKE) stage2
	@ $(MAKE) stage3

# ----------------------------------------------------------------------------
# Stage 1.
# Build Menhir using ocamlyacc.

stage1:
	@ $(OCAMLBUILD) -build-dir _stage1 menhir.$(TARGET)

# ----------------------------------------------------------------------------
# Stage 2.
# Build Menhir using Menhir (from stage 1).

# Do not use . to refer to the current directory, because ocamlbuild
# descends into another directory when executing commands.
# Do not use $(shell pwd) either, because this assumes we are running
# on a Unix platform, and can fail on Windows.
# So, use .., which works fine if ocamlbuild has effectively descended
# into a subdirectory.
SRC   := ..

FLAGS := -v -lg 1 -la 1 -lc 1 --table --infer --stdlib $(SRC) --strict --fixed-exception --canonical

stage2:
	@ $(OCAMLBUILD) -build-dir _stage2 -tag fancy_parser \
	    -use-menhir -menhir "$(SRC)/_stage1/menhir.$(TARGET) $(FLAGS)" \
	    menhir.$(TARGET)

# ----------------------------------------------------------------------------
# Stage 3 (optional).
# Re-generate Menhir's parser using Menhir (from stage 2) and check that it
# is identical to the stage 2 parser.
stage3:
	@ $(OCAMLBUILD) -build-dir _stage3 -tag fancy_parser \
	    -use-menhir -menhir "$(SRC)/_stage2/menhir.$(TARGET) $(FLAGS)" \
	    parser.ml parser.mli
	@ for i in parser.ml parser.mli ; do \
	    if ! diff _stage2/$$i _stage3/$$i 2>&1 >/dev/null ; then \
	      echo "Bootstrap FAILED: $$i did not reach a fixed point."; exit 1 ; \
	    fi ; \
	  done; \
	  echo "Bootstrap successful."

# -------------------------------------------------------------------------

# The ocamlbuild targets that should be used to build menhirSdk.

MENHIRSDK       := menhirSdk.cmo
ifneq ($(TARGET),byte)
MENHIRSDK       := $(MENHIRSDK) menhirSdk.cmx
endif

# ----------------------------------------------------------------------------
# Building menhirSdk.

sdk:
	@ $(OCAMLBUILD) \
	    -build-dir _sdk \
	    -tag sdk \
	    $(MENHIRSDK)

# ----------------------------------------------------------------------------
# Cleaning up.

clean::
	rm -rf _stage1 _stage2 _stage3 _sdk
