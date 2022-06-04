all: ms/LMA.bib ms/LMA.bib ms/LMAms_main.pdf ms/LMAms_main-diffa1d4c64.tex
dif: ms/LMAms_main-diffa1d4c64.tex

ms/LMA.bib: ~/LMA.bib
	cp $< $@

ms/LMAms_main.pdf: ms/LMAms_main.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/LMAms_main-diffa1d4c64.tex: ms/LMAms_main.tex
	latexdiff-vc --git -r a1d4c64 $^ ; \
	cd ms; pdflatex LMAms_main-diffa1d4c64.tex

ms/SI.docx: ms/SI.Rmd
	R -e 'system.time(rmarkdown::render("$<", "all"))'

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
