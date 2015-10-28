.PHONY: all clean

all: main.pdf

export TEXINPUTS=.:

%.pdf: %.tex $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty) $(wildcard *.mly)
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

clean:
	rm -f *.log *.aux *.bbl *.blg *.out *.toc *~ main.pdf

