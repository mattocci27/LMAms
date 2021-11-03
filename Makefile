RDA := rda/GL_Aps_LLs_obs.rda \
			rda/PA_Ap_LLs_opt_obs.rda \
  		rda/PA_Ap_LLs_opt_more_obs.rda
#
PAR = data/PApara.csv data/GLpara.csv data/GL_res.csv data/PA_res.csv para_val.yml

LOO = data/GL_elpd.csv data/PA_elpd.csv

DATA = data/GL_data.csv PA_data.csv PA_data_more.csv data/PA_LH.csv \
			 data/nature.csv

RAW = data-raw/nature02403-s2.csv data-raw/fiber_analysis.csv \
			data-raw/LFTRAITS.csv data-raw/LFTRAITS.csv \
			data-raw/Osnas2018_S1.csv

FIGdata = settings.yml r_val.yml letters.yml fig_theme.r data/GL_res.csv data/PA_res.csv data/GLpara.csv data/PApara.csv data/PA_LH.csv

FIG = figs/fig_hypo.png figs/GL_scatter.png figs/GL_NP.png figs/PA_scatter.png figs/PA_NPC.png figs/LL_plot.png figs/mass_prop_simple.png figs/mass_prop_simple2.png figs/box_main.png figs/box_SI.png figs/box_frac.png figs/box_frac2.png figs/box_cell.png figs/frac_paired.png figs/LMAp_paired.png figs/LMAps.png figs/LMAps95.png docs/figs.html

#$(info RDA: $(RDA))
#$(info PAR: $(PAR))

all: emptytarget0 emptytarget1 docs/model_selection.html emptytarget2 r_val.yml letters.yml var_val.yml emptytarget3 emptytarget4 ms/LMAms_main.tex ms/diff.tex ms/LMA.bib

ms/LMA.bib: ~/LMA.bib
	cp $< $@

$(DATA): emptytarget0
emptytarget0: util/data_clean.r $(RAW)
	Rscript $<
	touch $@

$(LOO): emptytarget1
emptytarget1: util/get_loo.r $(RDA)
	Rscript $<
	touch $@

docs/model_selection.html: docs/model_selection.Rmd $(LOO) $(RDA)
	R -e 'system.time(rmarkdown::render("$<", "all"))'
	touch $@

$(PAR): emptytarget2
emptytarget2: util/res_para.r $(RDA)
	Rscript $<
	touch $@

r_val.yml: util/r2_yml.r $(RDA)
	Rscript $<
	sed -i -e 's/NA/"NA"/g' $@
	rm r_val.yml-e

letters.yml: util/t_yml.r $(PAR)
	Rscript $<

var_val.yml: util/var.R $(RDA)
	Rscript $<

$(FIG): emptytarget3
emptytarget3: docs/figs.Rmd $(FIGdata)
	R -e 'system.time(rmarkdown::render("$<", "all"))'
	touch $@

data/loo.csv data/para_tbl.csv: emptytarget4
emptytarget4: util/tbl.R $(LOO) $(PAR)
	Rscript $<
	touch $@

#ms/LMAps_main.tex: ms/LMAps_main.Rmd $(LOO) $(PAR) r_val.yml
ms/LMAms_main.tex: ms/LMAms_main.Rmd r_val.yml var_val.yml data/loo.csv data/para_tbl.csv
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/diff.tex: ms/LMAms_main.tex ms/LMAms_main_old.tex
	#latexdiff  ms/LMAms_main_old.tex $< > $@
	latexdiff --flatten ms/LMAms_main_old.tex $< > $@
	pdflatex -halt-on-error $@ --output-directory=ms

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
	rm -rf docs/figs_cache
	rm -rf docs/figs_files
