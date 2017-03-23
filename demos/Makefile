# The following demos require menhirLib:
#   calc-incremental
#   calc-inspection
# The following demos require menhirSdk:
#   generate-printers

DEMOS := \
  calc \
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
