.PHONY: all loop clean

export TEXINPUTS=.:

DEPS = $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty) $(wildcard *.mly)

SED := $(shell if hash gsed 2>/dev/null ; then echo gsed ; else echo sed ; fi)

all: main.pdf main.html

%.pdf: %.tex $(DEPS)
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

main.html: main.tex $(DEPS) $(wildcard *.hva)
	hevea -fix main.tex
#
# Hevea interprets 'tabbing' environment in a way
# that creates spacing errors in the rendered output
# of "textual version of derivation trees": it
# asks for (padding:0px;) while the TeX rendering
# inserts spacing between columns. Change this
# to {padding:1px;}
	$(SED) -i -e "s/cellpadding0/cellpadding1/" main.html
#
# Hevea generates images main00{1,2,3}.png for the tikz pictures
# present in the manual. Rename them into manual-figure00{1,2,3}.png
# for consistency with the naming scheme of the deployed manual files.
	for f in 1 2 3; do \
	  mv main00$$f.png manual-figure00$$f.png; \
	done
	$(SED) --in-place 's/<img src="main/<img src="manual-figure/g' main.html

loop:
	latexmk -pdf -pvc main

clean:
	rm -f `cat .gitignore`
