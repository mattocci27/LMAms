GIT = 978c9633
GIT2 = 978c9633
COVER = ms/cover
MAIN = ms/LMAms_main
SI = ms/LMAms_SI

FIG_DIR = figs
FIG_DEST_DIR = submission-si/figs
MS_DIR = ms
MS_DEST_DIR = submission-si/ms

PDF_FILES = coef_sim_gl.pdf coef_sim_pa.pdf box_inter.pdf box_frac_de.pdf box_frac_pa.pdf mass_prop_sim.pdf mass_prop_comp.pdf gl_point_np2.pdf pa_point_npc_par.pdf ps_point.pdf

# EPS files (derived from PDF_FILES)
EPS_FILES = $(PDF_FILES:.pdf=.eps)

MS_FILES = LMA.bib LMAms_SI.tex LMAms_main.docx LMAms_main.pdf LMAms_SI.docx LMAms_SI.pdf

all: $(MAIN).pdf $(MAIN).docx $(SI).pdf $(SI).docx $(MAIN)_diff.pdf $(SI)_diff.pdf
diff: $(MAIN)_diff.pdf
diff2: $(SI)_diff.pdf
pdf: $(MAIN).pdf

# Target to copy all files
copy_files: convert_eps copy_figs copy_ms

# Target to convert PDFs to EPS
convert_eps:
	@for file in $(PDF_FILES); do \
		epsfile=$${file%.pdf}.eps; \
		pdftops -eps $(FIG_DIR)/$$file $(FIG_DIR)/$$epsfile; \
	done

# Target to copy EPS figures
copy_figs: convert_eps
	@mkdir -p $(FIG_DEST_DIR)
	@for file in $(EPS_FILES); do \
		cp $(FIG_DIR)/$$file $(FIG_DEST_DIR); \
	done

# Target to copy manuscript files
copy_ms:
	@mkdir -p $(MS_DEST_DIR)
	@for file in $(MS_FILES); do \
		cp $(MS_DIR)/$$file $(MS_DEST_DIR); \
	done

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

# because different quarto verions produce different latex files
$(MAIN)_diff.pdf: $(MAIN).tex
	git show $(GIT):$(MAIN).qmd > $(MAIN)_old.qmd
	sed -i 's/link-citations: yes/link-citations: true/' $(MAIN)_old.qmd
	quarto render ms/LMAms_main_old.qmd --to pdf && \
	cd ms && \
	latexdiff LMAms_main_old.tex LMAms_main.tex > LMAms_main_diff.tex && \
	xelatex LMAms_main_diff.tex
	rm $(MAIN)_old.*

$(SI)_diff.pdf: $(SI).tex
	git show $(GIT2):$(SI).qmd > $(SI)_old.qmd
	sed -i 's/link-citations: yes/link-citations: true/' $(SI)_old.qmd
	quarto render ms/LMAms_SI_old.qmd --to pdf && \
	cd ms && \
	latexdiff LMAms_SI_old.tex LMAms_SI.tex > LMAms_SI_diff.tex && \
	xelatex LMAms_SI_diff.tex
	rm $(SI)_old.*

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*

.PHONY: copy_files
