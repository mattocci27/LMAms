GIT = 621812a
GIT2 = 621812a
# GIT2 = 6f7f0dc
COVER = ms/cover
MAIN = ms/LMAms_main
SI = ms/LMAms_SI
# all: $(MAIN).pdf $(MAIN).docx $(SI).pdf $(SI).docx $(COVER).pdf $(COVER).docx
all: $(MAIN).pdf $(MAIN).docx $(SI).pdf $(SI).docx $(MAIN)_diff.pdf $(SI)_diff.pdf
diff: ms/LMAms_main-diff$(GIT).pdf
diff2: ms/LMAms_SI-diff$(GIT2).pdf

$(MAIN).pdf: $(MAIN).qmd
	quarto render $< --to pdf

$(MAIN).docx: $(MAIN).qmd ms/my_template.docx
	quarto render $< --to docx

$(SI).pdf: $(SI).qmd
	quarto render $< --to pdf

$(SI).docx: $(SI).qmd
	quarto render $< --to docx

# $(COVER).pdf: $(COVER).qmd
# 	quarto render $< --to pdf

# $(COVER).docx: $(COVER).qmd
# 	quarto render $< --to docx

# ms/LMA.bib: ~/LMA.bib
# 	cp $< $@

$(MAIN)_diff.pdf: ms/LMAms_main.tex
	latexdiff-vc --git --flatten --force -r $(GIT) $^ ; \
	cd ms; xelatex LMAms_main-diff$(GIT).tex ; \
	mv LMAms_main-diff$(GIT).pdf LMAms_main_diff.pdf; \
	rm LMAms_main-diff$(GIT).*

$(SI)_diff.pdf: ms/LMAms_SI.tex
	latexdiff-vc --git --flatten --force -r $(GIT2) $^ ; \
	cd ms; xelatex LMAms_SI-diff$(GIT2).tex ;  \
	mv LMAms_SI-diff$(GIT2).pdf LMAms_SI_diff.pdf; \
	rm LMAms_SI-diff$(GIT2).*

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
