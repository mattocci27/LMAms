#!/bin/bash

cp ~/LMA.bib ./ms/LMA.bib

#R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "bookdown::word_document2"))' && echo "word file completed!"

R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "bookdown::pdf_document2"))' && echo "PDF file completed!"

#nohup R -e 'system.time(rmarkdown::render("./ms/LMAms_main.rmd", "bookdown::html_document2"))' && echo "html file completed!" &

cd ms


pdflatex LMAms_main_old.tex

latexdiff LMAms_main_old.tex LMAms_main.tex > diff.tex

#
pdflatex -halt-on-error diff.tex
pdflatex diff.tex
