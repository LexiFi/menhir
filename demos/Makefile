# The following demos require menhirLib:
#   calc-incremental
#   calc-inspection

# The following demos require menhirSdk:
#   generate-printers

# The demos whose name ends in -dune require dune (a.k.a. jbuilder)
# and assume that Menhir is installed.

DEMOS := \
  calc \
  calc-two \
  calc-param \
  calc-incremental \
  calc-inspection \
  generate-printers \
#  calc-dune \             # this demo causes [make check] to fail
#  calc-incremental-dune \ # this demo causes [make check] to fail
#  calc-inspection-dune \  # this demo requires new Dune, not yet released

.PHONY: all clean

all clean::
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $@ ; \
	done

clean::
	/bin/rm -f *~ .*~
