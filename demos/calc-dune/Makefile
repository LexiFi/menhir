.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../find-menhir.sh)
endif

MENHIRFLAGS     := # --infer

MAIN            := calc

all:
	jbuilder build $(MAIN).exe

clean:
	jbuilder clean
	rm -f *~ .*~

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ./$(MAIN).exe
