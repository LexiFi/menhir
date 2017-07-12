.PHONY: all loop clean

export TEXINPUTS=.:

all: main.pdf

%.pdf: %.tex $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty) $(wildcard *.mly)
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

loop:
	latexmk -pdf -pvc main

clean:
	rm -f `cat .gitignore`
