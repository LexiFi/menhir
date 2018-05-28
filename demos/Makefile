# The following demos require menhirLib:
#   calc-incremental
#   calc-inspection
# The following demos require menhirSdk:
#   generate-printers
# The following demos require dune (a.k.a. jbuilder)
# and assume that Menhir is already installed:
#   calc-dune
#   calc-incremental-dune

DEMOS := \
  calc \
  calc-two \
  calc-param \
  calc-incremental \
  calc-inspection \
  generate-printers \

.PHONY: all clean realclean

all clean::
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $@ ; \
	done

clean::
	/bin/rm -f *~ .*~

realclean: clean
	make -C calc-dune clean
	make -C calc-incremental-dune clean
