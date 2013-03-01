.PHONY: all clean ignore

TESTS = tests/test_caml \
	   	tests/test_caml_cst \
	    tests/test_c \
		tests/test_mini

SRC = ../../../../../src
BIN = {cmi,cmo,cmx,o}
LIB = menhirLib.$(BIN)
GENERATED = $(TESTS:=/generated)
LIBS = $(GENERATED:=/$(LIB))


all:
	cd src && make
	for i in $(TESTS); do (mkdir -p $$i/generated); done
	for i in $(TESTS); do (bash -c "ln -sf $(SRC)/$(LIB) $$i/generated"); done
	

html: 
#	bash -c "gnuplot results/*.plot"
	bash -c "asciidoc -a toc results/report.txt"
	bash -c "mv results/report.html ."

clean:
	@echo "================================================================================"
	@echo "cleaning all temporary files, but leave results directory unchanged. type"
	@echo "    make cleanall"
	@echo "to remove all with results"
	@echo "================================================================================"
	for i in $(TESTS); do (rm -rf $$i/generated); done
	for i in $(TESTS); do (rm -rf $$i/traces); done
	rm -f results/report.txt report.html
	rm -f bench.log stdout stderr
	# remove obsolete directories
	rm -rf test1 testmini testcaml_cst test_good testcaml
	cd src && make clean

cleanall: clean
	@echo "cleaning all results and executable"
	rm -f bench
	rm -f results/*.data results/*.plot results/*.png results/report.html

# Propage les propriétés svn:ignore du répertoire courant vers tous les sous-répertoires.
ignore:
	svn propset svn:ignore "`svn propget svn:ignore .`" . -R
