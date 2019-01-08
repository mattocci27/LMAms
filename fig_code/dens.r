library(tidyverse)
library(stringr)
library(lazyeval)
library(scales)
library(cowplot)
library(ggrepel)
library(ggthemes)
library(rstan)

n_sample <- 100
settings <- yaml::yaml.load_file("settings.yml")
para <- yaml::yaml.load_file("parameter.yml")

source("fig_theme.r")

#g1 <- data.frame(summary(res)$summary)
#P_vec <- paste("p[", 1:n_sample, "]" ,sep = "")
#f <- g1[P_vec, "mean"]
#
#GL1 <- GL_sim_dat %>%
#  mutate(LMAp = LMA * f) %>%
#  mutate(LMAs = LMA - LMAp) %>%
#  mutate(model = "potLL")
#
#load("./data/GL1_pot_com_sim.rda")
#g1 <- data.frame(summary(res)$summary)
#f <- g1[P_vec, "mean"]
#
#GL2 <- GL_sim_dat %>%
#  mutate(LMAp = LMA * f) %>%
#  mutate(LMAs = LMA - LMAp) %>%
#  mutate(model = "pot_com")
#
#load("./data/GL1_pot_diff_sim.rda")
#g1 <- data.frame(summary(res)$summary)
#f <- g1[P_vec, "mean"]
#
#GL3 <- GL_sim_dat %>%
#  mutate(LMAp = LMA * f) %>%
#  mutate(LMAs = LMA - LMAp) %>%
#  mutate(model = "diff_com")
#
#GL <- bind_rows(GL1, GL2, GL3) 


para_GL <- para$GL %>% unlist
vline_dat <- data_frame(para = names(para_GL),
                        true_val = para_GL)

ex_GL <- function(res, model = "Non-hierachical") {
  data_frame(log_alpha = extract(res, pars = "log_alpha")[[1]],
        log_beta = extract(res, pars = "log_beta")[[1]],
        alpha2 = extract(res, pars = "alpha2")[[1]],
        beta2 = extract(res, pars = "beta2")[[1]],
        rp = extract(res, pars = "rp")[[1]],
        rs = extract(res, pars = "rs")[[1]],
        log_r = extract(res, pars = "log_r")[[1]],
        rho12 = extract(res, pars = "rho12")[[1]],
        rho13 = extract(res, pars = "rho13")[[1]],
        rho23 = extract(res, pars = "rho23")[[1]],
        sigma1 = extract(res, pars = "L_sigma[1]")[[1]] %>% sqrt,
        sigma2 = extract(res, pars = "L_sigma[2]")[[1]] %>% sqrt,
        sigma3 = extract(res, pars = "L_sigma[3]")[[1]] %>% sqrt) %>%
        mutate(model = model)
}

load("./data/GL1_potLL_sim.rda")
GL1 <- ex_GL(res, "f")

load("./data/GL1_pot_com_sim.rda")
GL2 <- ex_GL(res, "com")

load("./data/GL1_pot_diff_sim.rda")
GL3 <- ex_GL(res, "diff")

GL <- bind_rows(GL1, GL2, GL3) %>%
  gather("para", "val", 1:13)

para_plot <- ggplot(GL, aes(x = val, fill = model, colour = model)) +
  facet_wrap( ~ para, scale = "free") +
  geom_density(aes(y = 0.09 * ..density..), alpha = 0.3) +
  geom_vline(data = vline_dat, aes(xintercept = true_val)) +
  theme_LES()

my_ggsave("./figs/para_GL1.png", para_plot)

para_PA <- para$PA %>% unlist
vline_dat <- data_frame(para = names(para_PA),
                        true_val = para_PA) %>%
  filter(para != "log_site") %>%
  filter(para != "q") 


ex_PA <- function(res, model = "Non-hierachical") {
  data_frame(log_alpha = extract(res, pars = "log_alpha")[[1]],
        log_beta = extract(res, pars = "log_beta")[[1]],
        alpha2 = extract(res, pars = "alpha2")[[1]],
        rp = extract(res, pars = "rp")[[1]],
        rs = extract(res, pars = "rs")[[1]],
        log_r = extract(res, pars = "log_r")[[1]],
        rho12 = extract(res, pars = "rho12")[[1]],
        rho13 = extract(res, pars = "rho13")[[1]],
        rho23 = extract(res, pars = "rho23")[[1]],
        sigma1 = extract(res, pars = "L_sigma[1]")[[1]],
        sigma2 = extract(res, pars = "L_sigma[2]")[[1]],
        sigma3 = extract(res, pars = "L_sigma[3]")[[1]]) %>%
        mutate(model = model)
}

load("./data/PA_siteLL_sim.rda")
PA1 <- ex_PA(res, "f")

load("./data/PA_site_com_sim.rda")
PA2 <- ex_PA(res, "com")

#load("./data/PA_site_diff_sim.rda")
#PA3 <- ex_PA(res, "diff")

PA <- bind_rows(PA1, PA2) %>%
  gather("para", "val", 1:12)

para_plot <- ggplot(PA, aes(x = val, fill = model, colour = model)) +
  facet_wrap( ~ para, scale = "free") +
  geom_density(aes(y = 0.09 * ..density..), alpha = 0.3) +
  geom_vline(data = vline_dat, aes(xintercept = true_val)) +
  theme_LES()

my_ggsave("./figs/para_PA.png", para_plot)
