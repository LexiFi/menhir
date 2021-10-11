.PHONY: all test clean

DUNEBUILD := dune build --no-print-directory

all:
	@ $(DUNEBUILD) @all

test:
	@ $(DUNEBUILD) @test

include ../../../Makefile.image

clean:
	rm -rf $(IMAGE)
