#!/bin/bash

#cp ~/LMA.bib ./ms/LMA.bib

Rscript util/tbl.R

#R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "bookdown::word_document2"))' && echo "word file completed!"

#R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "bookdown::pdf_document2"))' && echo "PDF file completed!"
R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "all"))'

#R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "bookdown::pdf_document2"))'


cd ms

pdflatex -halt-on-error LMAms_main.tex
#pdflatex LMAms_main_old.tex

latexdiff --flatten LMAms_main_old.tex LMAms_main.tex > diff.tex
#
pdflatex -halt-on-error diff.tex
#pdflatex diff.tex





#sudo chown mattocci /Users/mattocci/Library/TinyTeX/texmf-var/ls-R
#chmod 666 /Users/mattocci/Library/TinyTeX/texmf-var/ls-R
