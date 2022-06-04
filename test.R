# sudo singularity build singularity.sif singularity.def

library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)

targets::tar_load(gl_csv)
targets::tar_load(gl_stan_dat)
targets::tar_load(pa_rand_list)

targets::tar_load(gl_rand_list)
targets::tar_load(gl_rand_fit)
dat <- gl_rand_list$data[[12]]
gl_rand_fit[[12]]$summary |> head(20)

lm(log(A) ~ log(LMA), dat) |> summary()
lm(log(LL) ~ log(LMA), dat) |> summary()



install.packages("bookdown")
install.packages("bs4Dash")
install.packages("gt")
install.packages("markdown")
install.packages("methods")
install.packages("pander")
install.packages("pbmcapply")
install.packages("pingr")
install.packages("plyr")
install.packages("png")
install.packages("shiny")
install.packages("shinybusy")
install.packages("shinyWidgets")
