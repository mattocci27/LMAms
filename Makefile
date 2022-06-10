all: ms/LMA.bib ms/LMAms_main.pdf ms/LMAms_main-diffa1d4c64.tex
ms: ms/LMAms_main.pdf ms/LMAms_SI.html ms/LMAms_SI.pdf
dif: ms/LMAms_main-diffa1d4c64.tex

ms/LMA.bib: ~/LMA.bib
	cp $< $@

ms/LMAms_main.pdf: ms/LMAms_main.qmd
	quarto render $< --to pdf

ms/LMAms_main.docx: ms/LMAms_main.qmd
	quarto render $< --to docx

ms/LMAms_SI.html: ms/LMAms_SI.qmd
	quarto render $< --to html

ms/LMAms_SI.docx: ms/LMAms_SI.qmd
	quarto render $< --to docx

ms/LMAms_SI.pdf: ms/LMAms_SI.qmd
	quarto render $< --to pdf

ms/LMAms_main-diffa1d4c64.tex: ms/LMAms_main.tex
	latexdiff-vc --git -r a1d4c64 $^ ; \
	cd ms; pdflatex LMAms_main-diffa1d4c64.tex

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
