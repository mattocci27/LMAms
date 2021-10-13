RDA := rda/GL_Aps_LLs_obs.rda \
			rda/PA_Ap_LLs_opt_obs.rda \
  		rda/PA_Ap_LLs_opt_more_obs.rda 
#
PAR = data/PApara.csv data/GLpara.csv data/GL_res.csv data/PA_para.csv

LOO = data/GL_elpd.csv data/PA_elpd.csv

#$(info RDA: $(RDA))
#$(info PAR: $(PAR))

all: emptytarget1 docs/model_selection.html emptytarget2 r_val.yml ms/LMAms_main.tex ms/LMA.bib

ms/LMA.bib: ~/LMA.bib
	cp $< $@

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

#ms/LMAms_main.tex: ms/LMAms_main.Rmd $(LOO) $(PAR) r_val.yml
ms/LMAms_main.tex: ms/LMAms_main.Rmd r_val.yml $(LOO)
	R -e 'system.time(rmarkdown::render("$<", "all"))'

ms/diff.tex: ms/LMAms_main.tex ms/LMAms_main_old.tex
	latexdiff --flatten ms/LMAms_main_old.tex $< > $@

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
