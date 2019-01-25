TDIR=thesis
FDIR=figures

_FIGURES = algeff-syntax algeff-infer algeff-infer-handlers algeff-red lc-syntax lc-red lc-red-example lc-cek
FIGURES = $(patsubst %,$(TDIR)/%.pdf,$(_FIGURES))

.PHONY : figures thesis clean

all: figures thesis

figures:
	algeff/render.rkt

$(TDIR)/%.pdf: $(FDIR)/%.eps
	epstool --copy -b $< --quiet --output - | epstopdf -f > $@

thesis: $(FIGURES)
	cd thesis && latexmk

clean:
	rm -f figures/* $(FIGURES)
	cd thesis && latexmk -c && rm -f thesis.bbl thesis.run.xml thesis.synctex.gz