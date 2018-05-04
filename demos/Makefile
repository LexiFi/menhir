# The following demos require menhirLib:
#   calc-incremental
#   calc-inspection
# The following demos require menhirSdk:
#   generate-printers
# The following demos require dune (a.k.a. jbuilder):
#   calc-dune

DEMOS := \
  calc \
  calc-two \
  calc-param \
  calc-incremental \
  calc-inspection \
  generate-printers \
  calc-dune \

.PHONY: all clean

all clean::
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $@ ; \
	done

clean::
	/bin/rm -f *~ .*~
