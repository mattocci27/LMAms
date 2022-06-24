GIT = a1d4c64
MAIN = ms/LMAms_main
SI = ms/LMAms_SI
all: $(MAIN).pdf $(MAIN).docx $(SI).html $(SI).docx
diff: ms/LMAms_main-diff$(GIT).tex

$(MAIN).pdf: $(MAIN).qmd $(SI).aux
	quarto render $< --to pdf

$(MAIN).docx: $(MAIN).qmd
	quarto render $< --to docx

$(SI).pdf $(SI).aux: $(SI).qmd
	quarto render $< --to pdf

$(SI).html: $(SI).qmd
	quarto render $< --to html

$(SI).docx: $(SI).qmd
	quarto render $< --to docx

# ms/LMA.bib: ~/LMA.bib
# 	cp $< $@

ms/LMAms_main-diff$(GIT).pdf: ms/LMAms_main.tex
	latexdiff-vc --git --flatten --force -r $(GIT) $^
#	cd ms; pdflatex LMAms_main-diff$(GIT).tex


.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
