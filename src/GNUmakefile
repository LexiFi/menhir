# This is a development Makefile, which is not shipped.

.PHONY: api clean

-include Makefile

# ----------------------------------------------------------------------------
# When developing, the standard library path is the current directory.
# ocamlfind is assumed.

installation.ml:
	echo "let libdir = \""`pwd`"/\"" > $@
	echo "let ocamlfind = true" >> $@

# ----------------------------------------------------------------------------
# Pretty-printing the MenhirLib.Convert API.

api: convert.mli.html IncrementalEngine.ml.html general.mli.html

%.mli.html: %.mli
	caml2html -nf $<

%.ml.html: %.ml
	caml2html -nf $<

clean::
	rm -f *.mli.html

