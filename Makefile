all: ./data/PApara.csv ./data/GLpara.csv ./ms/LMAms_main.tex ./ms/diff.tex ./ms/LMA.bib

./ms/LMA.bib: ~/LMA.bib
	cp ~/LMA.bib ./ms/LMA.bib

#./data/GL_elpd.csv ./data/PA_elpd.csv: ./rda/GL_Aps_LLs_obs.rda ./rda/PA_Ap_LLs_opt_obs.rda
#	Rscript util/get_loo.r

./data/PApara.csv ./data/GLpara.csv ./data/GL_res.csv ./data/PA_para.csv: ./rda/GL_Aps_LLs_obs.rda ./rda/PA_Ap_LLs_opt_obs.rda
	Rscript util/res_para.r

./ms/LMAms_main.tex: ./ms/LMAms_main.Rmd
	R -e 'system.time(rmarkdown::render("./ms/LMAms_main.Rmd", "all"))'

./ms/diff.tex: ./ms/LMAms_main.tex ./ms/LMAms_main_old.tex
	latexdiff --flatten ./ms/LMAms_main_old.tex ./ms/LMAms_main.tex > ./ms/diff.tex

.PHONY: clean
clean:
	rm -f ms/*.tuc \
	ms/*.log \
	rm -rf ms/cache/*
