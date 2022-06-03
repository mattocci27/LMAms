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


targets::tar_load(gl_csv)
data <- read_csv(gl_csv)
  tmp <- data.frame(data[,1:3],
           LMA = sample(data$LMA),
           #LMA = data$LMA,
           LL = sample(data$LL),
           Aarea = sample(data$Aarea),
           Rarea = sample(data$Rarea)
           )

a_pval <- cor.test(log(tmp$LMA), log(tmp$Aarea))$p.val
l_pval <- cor.test(log(tmp$LMA), log(tmp$LL))$p.val
r_pval <- cor.test(log(tmp$LMA), log(tmp$Rarea))$p.val

a_pval
l_pval
r_pval

gl_rand_fit[[3]]$summary
gl_rand_fit[[8]]$summary

gl_rand_fit[[9]]$summary |>
  filter(rhat > 1.05)

hoge <- gl_rand_fit[[3]]$draws
as <- hoge[, ,"as"]
as.numeric(as) |> quantile(0.975)

apply(as, 2, \(x)quantile(x, 0.975))

pmat <- hoge[,,paste0("p[",1:198,"]")]

targets::tar_load(pa_rand_fit)
targets::tar_load(pa_rand_list)

hoge <- pa_rand_fit[[3]]$draws
hoge[1,1,paste0("p[",1:106,"]")]

dim(hoge)
names(hoge[1,1,])
  as_tibble() |>
|> janitor::clean_names()

str(pa_rand_fit[[3]])
  draws <- draws |>
    janitor::clean_names()
  p_dat <- draws |>
    dplyr::select(contains("p_"))
  p_vec <- apply(p_dat, 2, mean)



hoge <- pa_rand_list$data[[3]]

plot(hoge$A, hoge$LMA, log = "xy")
plot(hoge$LL, hoge$LMA, log = "xy")
plot(hoge$R, hoge$LMA, log = "xy")

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

