GIT = ce3856c6
MAIN = ms/LMAms_main
SI = ms/LMAms_SI
all: $(MAIN).pdf $(MAIN).docx $(SI).pdf $(SI).docx
diff: ms/LMAms_main-diff$(GIT).pdf

$(MAIN).pdf: $(MAIN).qmd $(SI).aux
	quarto render $< --to pdf

$(MAIN).docx: $(MAIN).qmd
	quarto render $< --to docx

$(SI).pdf $(SI).aux: $(SI).qmd
	quarto render $< --to pdf

$(SI).docx: $(SI).qmd
	quarto render $< --to docx

# ms/LMA.bib: ~/LMA.bib
# 	cp $< $@

ms/LMAms_main-diff$(GIT).pdf: ms/LMAms_main.tex
	latexdiff-vc --git --flatten --force -r $(GIT) $^ ; \
	cd ms; pdflatex LMAms_main-diff$(GIT).tex


.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
