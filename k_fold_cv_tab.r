library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

get_elpd <- function(ss){
  ee <- extract_log_lik_K(ss, holdout_10)
  kk <- kfold(ee)
  kk$elpd_kfold
}

load("./rda/GL_LMAms_more_obs.rda")
fit_LMAms <- res
load("./data/GL_LMA_more_obs.rda")
fit_LMA <- res

loo_LMAms <- loo(fit_LMAms)
loo_LMA <- loo(fit_LMA)
loo_compare(loo_LMAms, loo_LMA)

log_lik_1 <- extract_log_lik(fit_LMAms, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_1), cores = 16) 
loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 16)

load("./rda/GL_LMAms_CV_obs_cv.rda")
(ee1 <- get_elpd(ss))
N2 <- N

load("./rda/GL_LMAms_CV2_obs_cv.rda")
(ee2 <- get_elpd(ss))

load("./rda/GL_LMAms_CV3_obs_cv.rda")
(ee3 <- get_elpd(ss))

load("./rda/GL_LMA_CV_obs_cv.rda")
(ee <- get_elpd(ss))


GL_tb <- tibble(LMA = c("LMA", "LMAms"),
           elpd = c(ee, ee2),
           N = c(N, N2)) %>%
  mutate(site = "GLOPNET")

write.csv(GL_tb, row.names = F, "./data/GL_elpd.csv")

load("./rda/PA_LMA_CV_obs_cv.rda")

ee <- get_elpd(ss)
N1 <- N

load("./rda/PA_LMA_L_CV_obs_cv.rda")

ee2 <- get_elpd(ss)
N2 <- N

load("./rda/PA_LMAms_CV_obs_cv.rda")

ee3 <- get_elpd(ss)
N3 <- N

load("./rda/PA_LMAms_L_CV_obs_cv.rda")
ee4 <- get_elpd(ss)
N4 <- N

load("./rda/PA_LD_L_CV_obs_cv.rda")
ee5 <- get_elpd(ss)
N5 <- N


PA_tb <- tibble(LMA = c("LMA",
                            "LMA_L",
                            "LMAms",
                            "LMAms_L",
                            "LD_L"),
           elpd = c(ee,
                    ee2,
                    ee3,
                    ee4,
                    ee5),
           N = c(N1,
                    N2,
                    N3,
                    N4,
                    N5),
                    ) %>%
  mutate(site = "Panama")

write.csv(PA_tb, row.names = F, "./data/PA_elpd.csv")
