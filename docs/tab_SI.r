library(tidyverse)
library(stringr)
library(rstan)
library(loo)

load("./data/GL_m0_N_obs.rda")
GL_waic <- waic(extract_log_lik(res,"log_lik"))$waic
GL_loo <- loo(extract_log_lik(res,"log_lik"))$estimates[1,1]
GL_n <-  extract_log_lik(res,"log_lik") %>% dim %>% .[2]

#load("./data/PA_m0_N_obs.rda")
load("./data/PA_m0_N_more_obs.rda")
PA0_waic <- waic(extract_log_lik(res,"log_lik"))$waic
PA0_loo <- loo(extract_log_lik(res,"log_lik"))$estimates[1,1]
#moge0 <- loo(extract_log_lik(res,"log_lik1"))
PA0_n <-  extract_log_lik(res,"log_lik") %>% dim %>% .[2]

load("./data/PA_m1q_more_N_obs.rda")
#load("./data/PA_m1q_more_obs.rda")
PA1more_waic <- waic(extract_log_lik(res,"log_lik"))$waic
PA1more_loo <- loo(extract_log_lik(res,"log_lik"))$estimates[1,1]
PA1more_n <-  extract_log_lik(res,"log_lik") %>% dim %>% .[2]

load("./data/PA_m1q_N_obs.rda")
#load("./data/PA_m1q_obs.rda")
PA1_waic <- waic(extract_log_lik(res,"log_lik"))$waic
PA1_loo <- loo(extract_log_lik(res,"log_lik"))$estimates[1,1]
PA1_n <-  extract_log_lik(res,"log_lik") %>% dim %>% .[2]

load("./data/PA_m1q_more_NS_more_obs.rda")
#load("./data/PA_m1q_obs.rda")
PA1NS_waic <- waic(extract_log_lik(res,"log_lik"))$waic
PA1NS_loo <- loo(extract_log_lik(res,"log_lik"))$estimates[1,1]
PA1NS_n <-  extract_log_lik(res,"log_lik") %>% dim %>% .[2]

load("./data/PA_m2q_N_obs.rda")
PA2_waic <- waic(extract_log_lik(res,"log_lik"))$waic
PA2_loo <- loo(extract_log_lik(res,"log_lik"))$estimates[1,1]
PA2_n <-  extract_log_lik(res,"log_lik") %>% dim %>% .[2]


v_waic <- c(GL_waic,
            PA0_waic,
            PA1_waic,
            PA1NS_waic,
            PA1more_waic,
            PA2_waic) %>%
  round(1)

v_loo <- c(GL_loo,
            PA0_loo,
            PA1_loo,
            PA1NS_loo,
            PA1more_loo,
            PA2_loo) %>%
  round(1)

v_n <- c(GL_n,
            PA0_n,
            PA1_n,
            PA1NS_n,
            PA1more_n,
            PA2_n) %>%
  round(1)


tab <- data_frame(Data = c("GLOPNET",
                    "Panama",
                    "Panama",
                    "Panama",
                    "Panama",
                    "Panama"
                    ),
           Model = c("Potential",
                     "Potential",
                     "Optimal",
                     "Optimal S",
                     "Optimal",
                     "Optimal"
                     ),
           LT = c("LMAs",
                     "LMAs",
                     "LMAs",
                     "LMAs",
                     "LMAs",
                     "LMAs/LT"
                     ),
           n.obs = v_n,
           WAIC = v_waic,
           elpd_loo =  v_loo
           )

DT::datatable(tab)

write.csv(tab, "./data/model_comp.csv", row.names = F)
