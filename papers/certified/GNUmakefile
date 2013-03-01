# ----------------------------------------------------------------------------
# Set any paper-specific variables.

include Makefile.local

# ----------------------------------------------------------------------------
# Eliminate the default known suffixes.

.SUFFIXES:

# ----------------------------------------------------------------------------
# Definitions.

PAPER := a4
# DVIPS_COMMON_OPT := -Pcmz -Pamz -e 0
HREF_PDF := \def\fppdf{true}

# ----------------------------------------------------------------------------
# Generic rules.

%.pdf: %.dvi
	dvipdfm -p $(PAPER) -o $@ $<

%.dvi: $(wildcard *.tex) $(wildcard *.sty) $(wildcard *.bib) $(GENERATED)
	latex "$(HREF_PDF)\\nonstopmode\\input" $(DOC).tex
	bibtex $(DOC)
	latex "$(HREF_PDF)\\nonstopmode\\input" $(DOC).tex
	latex "$(HREF_PDF)\\nonstopmode\\input" $(DOC).tex

%.tex: %.gtex
	gpic -t < $*.gtex > $*.tex

# ----------------------------------------------------------------------------
# Targets.

.PHONY: pdf clean log

.SECONDARY: $(GENERATED)

pdf: $(DOC).pdf

clean::
	rm -f *.aux *.log *.bbl *.blg *~ *.out *.thm
	rm -f $(DOC).dvi $(DOC).pdf

log: pdf
	./decrypt.sh $(DOC)

