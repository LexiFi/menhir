.PHONY: clean test expected

# Note that there is potential confusion between src/_stage1/menhir
# and src/_stage2/menhir. Here, we use the latter (built by "make
# bootstrap" in the src/ directory). But typing "make" in the src/
# directory re-builds only the former.

SHELL=/bin/bash
SRC=../../src
MENHIR=$(SRC)/_stage2/menhir.native --stdlib $(SRC)

# La convention de nommage des tests multi-grammaires
# est : /nom/-[1-9].mly
TESTS=$(shell ls | egrep '*([1]|[^2-9]).mly')
RESULTS=$(TESTS:.mly=.result)

test: clean $(RESULTS)
	@ if test -e failed_files; then		\
		echo FAILED ON: ;		\
		cat failed_files;		\
	 fi

%.result: %.mly
	@ if echo $< | grep -e "-1.mly" &> /dev/null; then				\
		export BASE=`echo $< | sed s/"-1.mly"/""/`;				\
		export FILES=`echo $$BASE-[1-9].mly`;					\
		export CMD="$$FILES --base $$BASE";					\
	else										\
		export FILES="$<";							\
		export CMD="$<";							\
	fi;										\
	if $(MENHIR) $$CMD >& /dev/null; then						\
	  echo "-> [KO] menhir should not accept $$FILES";				\
	  export FAILED_FILES="$$FAILED_FILES $$FILES";					\
	else										\
	  $(SHELL) -c '$(MENHIR) --only-preprocess $$CMD >& $@';				\
	  if [ x$$CREATE_EXPECTED == x"1" ]; then					\
		cp $@ $*.expected;							\
	  fi;										\
	  if [ x$$VERBOSE == x"1" ]; then						\
		echo "********************************************************";	\
		echo "* INPUT";								\
		echo "********************************************************";	\
		cat $<;									\
		echo "********************************************************";	\
		echo "* OUTPUT";							\
		echo "********************************************************";	\
		cat $@;									\
		echo "";								\
	  fi;										\
	  if test -e $*.expected; then							\
	     if diff $@ $*.expected >& /dev/null; then					\
		echo "[OK] $$FILES";							\
	     else									\
	        echo "-> [KO] menhir gives bad error message on $$FILES";		\
	        echo "  $$FILES" >> failed_files;					\
	     fi;									\
	   else										\
	     echo "[OK] $$FILES";							\
	   fi;										\
	fi

# TEMPORARY il faudrait pouvoir construire les fichiers .expected un par un

expected: 
	@ echo "Are you sure ? (this will erase *.expected) [Press ENTER]"
	@ read
	@ mkdir -p /tmp/mini-expected
	@ cp -f *.expected /tmp/mini-expected >& /dev/null || true
	@ CREATE_EXPECTED=1 $(MAKE) -s test
	@ echo "Expected output re-generated."

# Use "make -B foo.expected" to force the reconstruction.
%.expected:
	@ echo "Are you sure ? (this will erase $@) [Press ENTER]"
	@ read
	@ CREATE_EXPECTED=1 $(MAKE) $*.result

clean:
	rm -f *.ml *.mli *.conflicts *.automaton *.cmi *.cmo *.cmx *.o *.s *.result *~ failed_files
