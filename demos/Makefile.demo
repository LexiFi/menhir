# This Makefile is common to every demo.

# This is the name of dune's build directory.

BUILD := _build/default

# This definition computes the image of the current directory
# inside dune's _build directory.

IMAGE := $(shell \
	    prefix=""; suffix=""; \
	    while ! [ -f dune-project ]; do \
	      prefix="../$$prefix"; \
	      suffix="/$$(basename $$(pwd))$$suffix"; \
	      cd ..; \
	    done; \
	    echo "$${prefix}$(BUILD)$${suffix}")

# [make all] builds just this demo.

.PHONY: all
all:
	dune build @all

# [make test] tests just this demo.

.PHONY: test
test:
	dune build --force --display short @test

# [make clean] cleans up just this demo.

# Because [dune clean] does not seem to allow cleaning up just one directory,
# we manually remove the build artefacts for this demo.

.PHONY: clean
clean:
	rm -f *~
	rm -rf $(IMAGE)
