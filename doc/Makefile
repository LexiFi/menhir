.PHONY: all clean

all: main.pdf

export TEXINPUTS=.:

%.pdf: %.tex
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

clean:
	rm -f *.log *.aux *.bbl *.blg *.out *.toc *~

