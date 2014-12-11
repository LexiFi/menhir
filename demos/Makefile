DEMOS := calc calc-two calc-param calc-incremental

.PHONY: all clean

all:
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i ; \
	done

clean:
	/bin/rm -f *~ .*~ *.omc
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i clean ; \
	done
