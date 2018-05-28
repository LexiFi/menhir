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
  calc-dune \
  calc-incremental-dune \
  calc-inspection-dune \

.PHONY: all clean

all clean::
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $@ ; \
	done

clean::
	/bin/rm -f *~ .*~
