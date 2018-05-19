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

loop:
	latexmk -pdf -pvc main

clean:
	rm -f `cat .gitignore`
