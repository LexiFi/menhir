FOLDERS=\
	01-Pretty.results \
	02-Sexp.results \
	03-Sexpbis.results
EXTS=parsing.hopix sexp.hopix sexp.hopix

# Use GNU find.
FIND=$(shell if command -v gfind >/dev/null ; then echo gfind ; else echo find ; fi)

.PHONY: all clean test FAKE
.PRECIOUS: %.output %.expected %.score

all: all.results

clean:
	@rm -f all.results $(FOLDERS)
	@$(FIND) \( -name "*.output" -or -name "*.score" \) -exec rm {} \;

test: clean
	@$(MAKE) -s all

FAKE:

all.results: $(FOLDERS)
	@cat $^ > $@
	@printf "\033[1mHoublix Total: "
	@cat $@ | cut -d: -f2 | \
		awk -F "/" \
		'{ score += $$1; all += $$2 } END { print score "/" all }'
	@printf "\e[0m"

.SECONDEXPANSION:
%.results: $$(subst .expected,.score,$$(wildcard %/*.expected))
	@printf "$*: " > $@
	@awk '{ score += $$1; all += 1 } END { print score "/" all  }' $^ >> $@
	@printf "\033[1m`cat $@`\e[0m\n"

%.score: %.output %.expected
	@diff -uwB $^ 1>/dev/null 2>&1 && echo 1 > $@ || echo 0 > $@
	@if [ `cat $@` -ne 0 ]; then \
		true; \
	else \
		printf "\e[31m[KO] `basename $@ .score`\e[0m\n"; \
		diff -uwB $^; exit 0; \
	fi
%.output: %.parsing.hopix
	@$(EXE) --pretty $< >$@ 2>&1; exit 0

%.output: %.sexp.hopix
	@$(EXE) --sexp $< >$@ 2>&1; exit 0
