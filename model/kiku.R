library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 4)


dat <- read_csv("../data/PA_data_more.csv") %>%
  filter(!is.na(LMA)) %>%
  filter(!is.na(Aarea)) %>%
  filter(!is.na(Rarea)) %>%
  filter(!is.na(LL)) %>%
  as.data.frame %>%
  mutate(gr = paste(site, strata) %>% 
         as.factor %>%
         as.numeric) %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))

theta_lim <- dat %>%
  filter(strata == "UNDER") %>%
  mutate(q = Rarea/Aarea) %>%
  summarize(max(q)) %>%
  as.numeric()

list_dat <- list(N = nrow(dat),
                 LMA = dat[, "LMA"],
                 A = dat[, "Aarea"],
                 LL = dat[, "LL"],
                 R = dat[, "Rarea"],
                 theta_lim = theta_lim,
                 leaf = ifelse(dat$strata == "CAN", 1, 0),
                 dry = ifelse(dat$site == "PNM", 1, 0))


system.time(res <- stan(file = "../model/kiku.stan",
           data = list_dat,
           iter = 2000,
           warmup = 1000,
           thin = 1,
           chains = 4,
           control = list(adapt_delta = 0.95, max_treedepth = 20)))


mu <- rstan::extract(res, "mu")[[1]]
p <- rstan::extract(res, "p")[[1]]

mu_mean <- apply(mu, 2, mean)
apply(p, 2, mean) |> hist()

dat |>
  mutate(LLpred = exp(mu_mean)) |>
  ggplot(aes(x = LL, y = LLpred, col = site_strata)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ggsma::geom_sma(aes(col = FALSE)) +
  scale_x_log10() +
  scale_y_log10()

dat |>
  mutate(LLpred = exp(mu_mean)) |>
  ggplot(aes(x = LL, y = LLpred, col = LMA)) +
  geom_point() +
  scale_color_viridis_c(trans = "log10") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
 # ggsma::geom_sma() +
  scale_x_log10() +
  scale_y_log10()

tmp <- tibble(p = as.numeric(p),
              sp = rep(1:130, each = 4000),
              site_strata = rep(dat$site_strata, each = 4000),
              LMA = rep(dat$LMA, each = 4000),
              ) |>
  mutate(LMAs = LMA * (1 - p))

tmp |>
  filter(sp <= 20) |>
  ggplot(aes(p, fill = site_strata)) +
  facet_wrap(~ sp) +
  geom_histogram(position = "identity", alpha = 0.6)

tmp |>
  filter(sp <= 20) |>
  ggplot(aes(LMAs, fill = site_strata)) +
  facet_wrap(~ sp) +
  scale_x_log10() +
  geom_histogram(position = "identity", alpha = 0.6)

smatr::sma(mu_mean ~ log(dat$LL))

cor.test(log(dat$LL), mu_mean)
