# to get dependency in fig.rmd
library(tidyverse)
Lines <-readLines("docs/figs.Rmd")
Lines2 <- Lines[!str_detect(Lines, "^#")]

dep <- Lines2[str_detect(Lines2, "read_csv|read.csv|source|load")]
dep2 <- str_split(dep, '"')
dep3 <- sapply(dep2, "[", 2)
dep4 <- str_split_fixed(dep3, "\\.\\./", 2)[,2] |>
  paste(collapse = " ")
print(dep4)

target <- Lines2[str_detect(Lines2, "ggsave|write.csv|save.image")]
target2 <- str_split(target, '"')
target3 <- sapply(target2, "[", 2)
target4 <- str_split_fixed(target3, "\\.\\./", 2)[,2] |> 
  paste(collapse = " ")
print(target4)
