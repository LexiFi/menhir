.PHONY: all loop clean

export TEXINPUTS=.:

TEXDEPS = $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty) $(wildcard *.mly)

all: main.pdf

%.pdf: %.tex $(DEPS)
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

main.html: main.tex $(DEPS) $(wildcard *.hva)
	hevea -fix main.tex
	@# Hevea interprets 'tabbing' environment in a way
	@# that creates spacing errors in the rendered output
	@# of "textual version of derivation trees": it
	@# asks for (padding:0px;) while the TeX rendering
	@# inserts spacing between columns. Change this
	@# to {padding:1px;}
	sed -i s/cellpadding0/cellpadding1/ main.html

loop:
	latexmk -pdf -pvc main

clean:
	rm -f `cat .gitignore`
