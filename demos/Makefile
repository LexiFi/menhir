# The following demos require menhirLib:
#   calc-incremental
#   calc-inspection

# The following demos require menhirSdk:
#   generate-printers

DEMOS := \
  calc \
  calc-alias \
  calc-two \
  calc-param \
  calc-incremental \
  calc-inspection \
  generate-printers \

.PHONY: all clean

all clean::
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $@ ; \
	done

clean::
	/bin/rm -f *~ .*~

# The demos whose name ends in -dune require dune
# and assume that Menhir is installed. For now,
# they are not built by default. (TODO)

EXTRA := \
  calc-dune \
  calc-param-dune \
#  calc-two-dune \
#  calc-incremental-dune \
#  calc-inspection-dune \
#  generate-printers-dune \

.PHONY: extra
extra::
	@for i in $(EXTRA) ; do \
	  $(MAKE) -C $$i all ; \
	done
