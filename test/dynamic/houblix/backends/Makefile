BACKENDS := $(patsubst %.flags,%,$(wildcard *.flags))

.PHONY: all
all:
	@ rm -rf *.backend
	@ for b in $(BACKENDS) ; do \
	    mkdir $$b.backend && \
	    sed -e "s/FLAGS/$$(cat $$b.flags)/" TEMPLATE > $$b.backend/dune ; \
	  done
