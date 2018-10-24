SHELL := /bin/bash

.PHONY: test clean

test:
	@ ./test.sh

clean:
	cd calc-data && rm -f `cat .gitignore`
	make -C calc clean
	make -C gene clean realclean
