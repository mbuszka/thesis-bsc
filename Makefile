TDIR=thesis
FDIR=figures

_FIGURES = algeff-syntax algeff-infer algeff-infer-handlers algeff-free algeff-red algeff-am-a algeff-am-b algeff-am-c algeff-am-e algeff-am-initial-conf algeff-am-syntax lc-syntax lc-red algeff-unify algeff-unify-row
# lc-red-example lc-cek
FIGURES = $(patsubst %,$(TDIR)/%.pdf,$(_FIGURES))

.PHONY : figures thesis clean

all: figures thesis

figures:
	algeff/render.rkt

$(TDIR)/%.pdf: $(FDIR)/%.eps
	epstool --copy -b $< --quiet --output - | epstopdf -f > $@

thesis: $(FIGURES)
	cd thesis && latexmk -bibtex -xelatex -interaction=nonstopmode --shell-escape

clean:
	rm -f figures/* $(FIGURES)
	cd thesis && latexmk -c && rm -f thesis.bbl thesis.run.xml thesis.synctex.gz