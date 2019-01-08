rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
library(loo)

# PA sim ---------------------------------------------------------------
load("~/Dropbox/LES/PA_sim.RData")
LL_vec <- paste("mu[", 1:100, ",2]" ,sep = "")

t_cor <- cor(log_mu2, log(LL))


est <- cor(m3[LL_vec, "mean"], log(LL))
up <- cor(m3[LL_vec, "X97.5."], log(LL))
lo <- cor(m3[LL_vec, "X2.5."], log(LL))



est_cor_A_PA <- mean(obs_cor_A)
est_cor_LL_PA <- mean(obs_cor_LL)
# est_cor_LL_PA <- cor(m3[LL_vec, "mean"], log(LL))

est_cor_R_PA <- mean(obs_cor_R)

#null PA ================================================================
load("~/Dropbox/LES/PA_sim_null.RData")
