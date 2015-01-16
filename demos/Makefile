DEMOS := calc calc-two calc-param calc-incremental calc-inspection

.PHONY: all clean

all:
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i ; \
	done

clean:
	/bin/rm -f *~ .*~
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i clean ; \
	done
