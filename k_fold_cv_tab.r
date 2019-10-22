library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)

get_elpd <- function(ss){
  ee <- extract_log_lik_K(ss, holdout_10)
  kk <- kfold(ee)
  kk$elpd_kfold
}

load("./data/GL_LMAms_CV_obs_cv.rda")


ee2 <- get_elpd(ss)
N2 <- N


load("./data/GL_LMA_CV_obs_cv.rda")

ee <- get_elpd(ss)


GL_tb <- data_frame(LMA = c("LMA", "LMAms"),
           elpd = c(ee, ee2),
           N = c(N, N2)) %>%
  mutate(site = "GLOPNET")

write.csv(GL_tb, row.names = F, "./data/GL_elpd.csv")

load("./data/PA_LMA_CV_obs_cv.rda")

ee <- get_elpd(ss)

load("./data/PA_LMA_L_CV_obs_cv.rda")

ee2 <- get_elpd(ss)
N2 <- N

load("./data/PA_LMAms_CV_obs_cv.rda")

ee3 <- get_elpd(ss)
N3 <- N

load("./data/PA_LMAms_L_CV_obs_cv.rda")
ee4 <- get_elpd(ss)
N4 <- N

load("./data/PA_LD_L_CV_obs_cv.rda")
ee5 <- get_elpd(ss)
N5 <- N


PA_tb <- data_frame(LMA = c("LMA",
                            "LMA_L",
                            "LMAms",
                            "LMAms_L",
                            "LD_L"),
           elpd = c(ee,
                    ee2,
                    ee3,
                    ee4,
                    ee5),
           N = c(N,
                    N2,
                    N3,
                    N4,
                    N5),
                    ) %>%
  mutate(site = "Panama")

write.csv(PA_tb, row.names = F, "./data/PA_elpd.csv")
