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
targets::tar_load(gl_rand_sig)
targets::tar_load(gl_rand_check)
targets::tar_load(pa_rand_sig)
targets::tar_load(pa_rand_check)
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

#' @para para parameter name (e.g., "ap")
#' @para rand_fit e.g., gl_rand_fit
#' @para n sim ID
#' @example
#' list("a0", "ap", "as", "b0", "bs", "g0", "gp", "gs") |>
#'  map_dfr(rand_summary, gl_rand_fit, 6)
rand_summary <- function(para, rand_fit, n) {
  tmp <- rand_fit[[n]]$draws
  para_dbl <- tmp[, ,para] |> as.numeric()
  tibble(para = para,
    mean = mean(para_dbl),
    lwr = quantile(para_dbl, 0.025),
    upr = quantile(para_dbl, 0.975),
    sim_id = n) |>
    mutate(sig = ifelse(lwr * upr > 0, "sig", "ns"))
}


hoge <- gl_rand_sig  |>
 # filter(!str_detect(para, "0")) |>
  full_join(gl_rand_check, by = "sim_id") |>
  mutate(cov = ifelse(rhat == 0, "converged", "diverged")) |>
  mutate(sim_id_chr = paste0("sim-", sim_id))

ggplot(hoge) +
  geom_pointrange(aes(x = sim_id_chr, y = mean, ymin = lwr, ymax = upr, group = sim_id, col = cov),
    position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~para) +
  coord_flip()

hoge <- gl_rand_sig  |>
 # filter(!str_detect(para, "0")) |>
  full_join(gl_rand_check, by = "sim_id") |>
  mutate(cov = ifelse(rhat == 0, "Converged", "Not converged")) |>
  mutate(cov = factor(cov, levels = c("Not converged", "Converged"))) |>
  mutate(sim_id_chr = paste0("sim-", sim_id)) |>
  mutate(para = case_when(
    para == "a0" ~ "alpha[0]",
    para == "ap" ~ "alpha[p]",
    para == "as" ~ "alpha[s]",
    para == "b0" ~ "beta[0]",
    para == "bs" ~ "beta[s]",
    para == "g0" ~ "gamma[0]",
    para == "gp" ~ "gamma[p]",
    para == "gs" ~ "gamma[s]",
    TRUE ~ para
  ))
ggplot(hoge) +
  geom_pointrange(aes(x = sim_id_chr,
   y = mean, ymin = lwr, ymax = upr, group = sim_id, col = cov)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~para, scale = "free", labeller = label_parsed) +
  xlab("Randomized ID") +
  ylab("Standradized coefficents") +
  coord_flip() +
  theme_bw() +
  theme(
     legend.position = c(0.8, 0.2),
     legend.title = element_blank()
  )
