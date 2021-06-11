BACKENDS := $(patsubst %.flags,%,$(wildcard *.flags))

.PHONY: all
all:
	@ for b in $(BACKENDS) ; do \
	    mkdir -p $$b.backend && \
	    sed -e "s/FLAGS/$$(cat $$b.flags)/" TEMPLATE > $$b.backend/dune ; \
	  done

.PHONY: clean
clean:
	@ rm -rf *.backend
