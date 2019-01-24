TDIR=thesis
FDIR=figures

_FIGURES = algeff-syntax algeff-infer algeff-red lc-syntax lc-red lc-red-example lc-cek
FIGURES = $(patsubst %,$(TDIR)/%.pdf,$(_FIGURES))

.PHONY : figures thesis clean

figures:
	algeff/render.rkt

$(TDIR)/%.pdf: $(FDIR)/%.eps
	epstool --copy -b $< --quiet --output - | epstopdf -f > $@

thesis: $(FIGURES)
	cd thesis && latexmk

clean:
	rm -f figures/* $(FIGURES)
	cd thesis && latexmk -c && rm thesis.bbl thesis.run.xml thesis.synctex.gz