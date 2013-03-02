.PHONY: everyday library bootstrap stage1 stage2 stage3 clean

# ----------------------------------------------------------------------------
# Choose a target.

ifndef TARGET
  TARGET := native
endif

# ----------------------------------------------------------------------------
# Define the files that form the library.

ifeq ($(TARGET),byte)
  LIBFILES := menhirLib.cmo
else
  LIBFILES := menhirLib.cmo menhirLib.cmx
endif

# ----------------------------------------------------------------------------
# Ocamlbuild tool and settings.

OCAMLBUILD := ocamlbuild -classic-display -use-ocamlfind -j 0

# ----------------------------------------------------------------------------
# For everyday development.
# Typing "make" will perform just stage 1. This is enough to ensure that
# the source code is correct.

everyday: installation.ml stage1

# ----------------------------------------------------------------------------
# Building MenhirLib.

library:
	$(OCAMLBUILD) $(LIBFILES)

# ----------------------------------------------------------------------------
# Building Menhir from scratch (a.k.a. bootstrapping).

bootstrap: .versioncheck stage1 stage2 stage3

# ----------------------------------------------------------------------------
# Checking the version of the OCaml compiler.

.versioncheck:
	@ echo Checking that Objective Caml is recent enough...
	@$(OCAMLBUILD) -build-dir _stage1 checkOCamlVersion.byte
	@ _stage1/checkOCamlVersion.byte --verbose --gt "3.09"
	@ touch $@

# ----------------------------------------------------------------------------
# Stage 1.
# Build Menhir using ocamlyacc.

stage1:
	@$(OCAMLBUILD) -build-dir _stage1 -tag yacc_parser \
	  menhir.$(TARGET)

# ----------------------------------------------------------------------------
# Stage 2.
# Build Menhir using Menhir (from stage 1).

FLAGS := -v -lg 1 -la 1 -lc 1 --comment --infer --error-recovery --stdlib . --strict --fixed-exception

stage2:
	@$(OCAMLBUILD) -build-dir _stage2 -tag fancy_parser \
	  -use-menhir -menhir "$(shell pwd)/_stage1/menhir.$(TARGET) $(FLAGS)" \
	  menhir.$(TARGET)

# ----------------------------------------------------------------------------
# Stage 3 (optional).
# Re-generate Menhir's parser using Menhir (from stage 2) and check that it
# is identical to the stage 2 parser.
stage3:
	@$(OCAMLBUILD) -build-dir _stage3 -tag fancy_parser \
	  -use-menhir -menhir "$(shell pwd)/_stage2/menhir.$(TARGET) $(FLAGS)" \
	  parser.ml parser.mli
	@for i in parser.ml parser.mli ; do \
	  if ! diff _stage2/$$i _stage3/$$i 2>&1 >/dev/null ; then \
	    echo "Bootstrap FAILED: $$i did not reach a fixed point."; exit 1 ; \
	  fi ; \
	done; \
	echo "Bootstrap successful."

# ----------------------------------------------------------------------------
# Cleaning up.

clean::
	rm -rf .versioncheck _build _stage1 _stage2 _stage3
