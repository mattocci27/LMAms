library(rmarkdown)
#system.time(render("./ms/LMAps_main.rmd", "bookdown::pdf_book"))
#system.time(render("./ms/LMAps_main_re.rmd", "bookdown::pdf_book"))
system.time(render("./ms/LMAms_main_re.rmd", "bookdown::word_document2"))
system.time(render("./ms/LMAms_main_re.rmd", "html_document"))
system.time(render("./ms/SI.rmd", "html_document"))

bookdown::render_book(input = "./ms/SI/index.rmd", output_format = "bookdown::pdf_document2")
bookdown::render_book("./ms/SI/index.rmd", output_format = "bookdown::word_document2")

bookdown::render_book(input = "index.rmd", 
                      output_format = "bookdown::word_document2")
