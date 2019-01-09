#!/bin/bash

cp ~/Dropbox/library.bib ./ms/ref.bib

R -e 'system.time(render("./ms/LMAps_main_re.rmd", "bookdown::word_document2"))' && echo "word file completed!"

#R -e 'system.time(render("./ms/LMAps_main_re.rmd", "bookdown::pdf_book"))' && echo "PDF file completed!"

nohup R -e 'system.time(render("./ms/LMAps_main_re.rmd", "html_document"))' && echo "html file completed!" &

cd ms

#latexdiff LMAps_main.tex LMAps_main_re.tex > diff.tex

#pdflatex -halt-on-error diff.tex
#pdflatex diff.tex