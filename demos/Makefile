.PHONY: all clean test

all:
	@ dune build @all
	@ $(MAKE) --no-print-directory -C coq-minicalc $@
	@ $(MAKE) --no-print-directory -C coq-syntax-errors $@

clean:
	@ $(MAKE) --no-print-directory -C coq-minicalc $@
	@ $(MAKE) --no-print-directory -C coq-syntax-errors $@


test:
	@ dune build --force @test
	@ $(MAKE) --no-print-directory -C coq-syntax-errors $@

