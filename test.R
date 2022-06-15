# sudo singularity build singularity.sif singularity.def

library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)

hoge |>
  filter(!is.na(warnings)) |>
  tail()

targets::tar_load(gl_rand_list)
d <- gl_rand_list$data[[10]]

lm(log(A) ~ log(LMA), d) |> summary()
lm(log(LL) ~ log(LMA), d) |> summary()
lm(log(R) ~ log(LMA), d) |> summary()

plot(R ~ LMA, log = "xy", data = d)

lm(log(A) ~ log(LL), d) |> summary()
lm(log(A) ~ log(R), d) |> summary()
lm(log(LL) ~ log(R), d) |> summary()

targets::tar_load(gl_csv)
targets::tar_load(gl_stan_dat)
targets::tar_load(pa_rand_list)
fit_7_mcmc_GL_Aps_LLs
targets::tar_load(fit_7_summary_GL_Aps_LLs)

targets::tar_load(shade_mass_prop_mv)

shade_mass_prop_mv |> mutate(site = "mv")

sim_dat <- gl_mass_prop_grad_ap
sim_dat |>
  mutate(ap_fac = (bquote(alpha~"ab")))
ap <- sim_dat$ap |> unique()

p1 <- ggplot(data = ap_sim_dat) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = factor(ap)),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = factor(ap))) +
    labs(
      color = expression(alpha[p]),
      fill = expression(alpha[p]))

p2 <- ggplot(data = as_sim_dat) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = factor(as)),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = factor(as))) +
    labs(
      color = expression(alpha[s]),
      fill = expression(alpha[s]))



    scale_color_manual(
      values = c("1", "2", "3"),
      labels = c(
        as.expression(bquote(alpha[p] == .(ap[1]))),
        as.expression(bquote(alpha[p] == .(ap[2]))),
        as.expression(bquote(alpha[p] == .(ap[3])))
    )) +
    scale_fill_manual(
      values = c("1", "2", "3"),
      labels = c(
        as.expression(bquote(alpha[p] == .(ap[1]))),
        as.expression(bquote(alpha[p] == .(ap[2]))),
        as.expression(bquote(alpha[p] == .(ap[3])))
    ))


targets::tar_load(gl_mass_prop_grad_as)
sim_dat2 <- gl_mass_prop_grad_as
as <- sim_dat2$as |> unique()

ggplot(data = sim_dat2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = as.factor(para_id)),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = as.factor(para_id))) +
    scale_color_manual(
      values = c("1", "2", "3"),
      labels = c(
        as.expression(bquote(alpha[s] == .(as[1]))),
        as.expression(bquote(alpha[s] == .(as[2]))),
        as.expression(bquote(alpha[s] == .(as[3])))
    )) +
    scale_fill_manual(
      values = c("1", "2", "3"),
      labels = c(
        as.expression(bquote(alpha[s] == .(as[1]))),
        as.expression(bquote(alpha[s] == .(as[2]))),
        as.expression(bquote(alpha[s] == .(as[3])))
    ))



fit_7_summary_GL_Aps_LLs |>
  filter(str_detect(variable, "rho"))

targets::tar_load(gl_rand_list)
targets::tar_load(gl_rand_fit)
targets::tar_load(gl_rand_sig)
targets::tar_load(gl_rand_check)
targets::tar_load(pa_rand_sig)
targets::tar_load(pa_rand_check)
dat <- gl_rand_list$data[[10]]
gl_rand_fit[[10]]$summary |> head(20)

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
