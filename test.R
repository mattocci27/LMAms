# sudo singularity build singularity.sif singularity.def

library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)

targets::tar_load(gl_csv)
targets::tar_load(gl_stan_dat)
targets::tar_load(pa_rand_list)
targets::tar_load(pa_rand_fit)

pa_rand_fit

# pa_rand_list
# pa_rand_list

list_dat <- gl_stan_dat
dat <- read_csv(gl_csv) %>%
    mutate(DE = ifelse(is.na(DE), "U", DE)) %>%
    as.data.frame()

hoge <- list()
for (i in 1:3) hoge[[i]] <- rand_fun(dat)

moge <- tibble(hoge, rep = 1:3)
tar_group_by(hello, moge, rep)

targets::tar_load(pa_rand_fit)
pa_rand_fit

targets::tar_load(gl_rand_fit)
gl_rand_fit

rand_dat <- rand_dat %>%
  mutate(data = map(1:n_rand, rand_fun, ))

rand_fun <- function(n, dat, list_dat){
  temp <- data.frame(dat[,1:3],
           LMA = sample(dat$LMA),
           #LMA = dat$LMA,
           LL = sample(dat$LL),
           Aarea = sample(dat$Aarea),
           Rarea = sample(dat$Rarea)
           )
  while (min(temp$Aarea - temp$Rarea) < 0){
  temp <- data.frame(dat[,1:3],
           LMA = sample(dat$LMA),
           #LMA = dat$LMA,
           LL = sample(dat$LL),
           Aarea = sample(dat$Aarea),
           Rarea = sample(dat$Rarea)
           )
  }

  temp$A_R <- temp$A - temp$R

  list(N = list_dat$N,
            A = temp$Aarea,
            LL = temp$LL,
            R = temp$Rarea,
            q_lim = list_dat$q_lim,
            leaf = list_dat$leaf,
            dry = list_dat$dry,
            DE = list_dat$DE,
            LMA = temp$LMA)
}


targets::tar_read(fit_1_summary_GL_Aps_LLs)
targets::tar_read(fit_1_diagnostics_GL_Aps_LLs)
targets::tar_load(fit_1_diagnostics_GL_Aps_LLs)


fit_1_diagnostics_GL_Aps_LLs |> summary()


library(jsonlite)

model <- fromJSON("templates/model.json")$config

model_n <- nrow(model)

model <- model |>
  mutate(model2 = paste("fit", 1:model_n, sep = "_")) |>
  mutate(stan = paste0("stan/", model, ".stan")) |>
  mutate(data = ifelse(site == "GL", "gl_stan_dat", "pa_stan_dat"))


tmp <- "templates/tar_stan_mcmc.txt"
for (i in 1:nrow(model)) {
  if (i == 1) {
    write_lines("  tar_stan_mcmc(", tmp, append = FALSE)
  } else {
    write_lines("  tar_stan_mcmc(", tmp, append = TRUE)
  }
  write_lines(
    paste0("    ", model$model2[i]), tmp, ",\n",
    append = TRUE
  )
  write_lines(
    paste0('    "', model$stan[i]), tmp, '",\n',
    append = TRUE
  )
  write_lines(
    paste0("    data = ", model$data[i]), tmp, ",\n",
    append = TRUE
  )
  write_lines('    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1,
    iter_sampling = 1,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),',
    tmp,
    append = TRUE
  )
}

model4 <- model3 |>
  mutate(hoge = paste(fit, "summary", model, sep = "_"))

