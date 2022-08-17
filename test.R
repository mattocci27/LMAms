library(tidyverse)
targets::tar_load(loo_model)

names(loo_model)
sapply(loo_model, "[[", "looic")
