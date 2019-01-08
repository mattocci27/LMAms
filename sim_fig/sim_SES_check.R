rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
load("~/Dropbox/LES/PA_sim.RData")
n_sample <- 100
P_vec <- paste("p[", 1:n_sample, "]" ,sep = "")
LL_vec <- paste("mu[", 1:n_sample, ",2]" ,sep = "")

obs_cor_A <- NULL
obs_cor_LL <- NULL
obs_cor_R <- NULL

true_cor_A_PA <- cor(log(LMAp), log(A))
true_preLL_PA <- cor(log_mu2, log(LL))

true_cor_LL_PA <- cor(log(LMAs), log(LL))

true_cor_R_PA <- cor(log(LMAp), log(R))

p_est <- extract(res3, pars = P_vec)
LL_est <- extract(res3, pars = LL_vec)

obs_preLL <- NULL
for (i in 1:1000){
  temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
  temp2 <- sapply(LL_est, function(x)sample(x,1)) %>% as.numeric
  LMAp_temp <- temp * LMA
  LMAs_temp <- LMA - LMAp_temp
  obs_cor_A[i] <- cor(log(LMAp_temp), log(A))
  obs_cor_LL[i] <- cor(log(LMAs_temp), log(LL))
  obs_cor_R[i] <- cor(log(LMAp_temp), log(R))
  obs_preLL[i] <- cor(log(LL), temp2)
}

est_cor_A_PA <- mean(obs_cor_A)
est_cor_LL_PA <- mean(obs_cor_LL)
# est_cor_LL_PA <- cor(m3[LL_vec, "mean"], log(LL))

est_cor_R_PA <- mean(obs_cor_R)
est_preLL_PA <- mean(obs_preLL)


temp <- m3[P_vec, "mean"]
LMAp_temp <- temp * LMA
LMAs_temp <- LMA - LMAp_temp

# cor.test(log(LMAp_temp), log(A))

temp2 <- m3[LL_vec, "mean"]

cor.test(log(LL), temp2)

#-----------
load("~/Dropbox/LES/PA_sim_null.RData")
cor_A_m <- NULL
cor_LL_m <- NULL
cor_R_m <- NULL
cor_preLL_m <- NULL

before <- proc.time()
p_est <- list()
LL_est <- list()
for (i in 1:10){
  cor_A <- NULL
  cor_LL <- NULL
  cor_R <- NULL
  preLL_n <- NULL
  p_est <- extract(res3[[i]], pars = P_vec)
  LL_est <- extract(res3[[i]], pars = LL_vec)
  for (j in 1:1000){
    temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
    temp2 <- sapply(LL_est, function(x)sample(x,1)) %>% as.numeric
    LMAp_temp <- temp * LMA.rc.data[,i]
    LMAs_temp <- LMA.rc.data[, i] - LMAp_temp
    cor_A[j] <- cor(log(LMAp_temp), log(A))
    cor_LL[j] <- cor(log(LMAs_temp), log(LL))
    cor_R[j] <- cor(log(LMAp_temp), log(R))
    preLL_n[j] <- cor(log(LL), temp2)
  }
  cor_A_m <- cbind(cor_A_m, cor_A)
  cor_LL_m <- cbind(cor_LL_m, cor_LL)
  cor_R_m <- cbind(cor_R_m, cor_R)
  cor_preLL_m <- cbind(cor_preLL_m, preLL_n)
}

null_cor_preLL_PA <- apply(cor_preLL_m, 1, mean) %>% mean

preLLn2 <- NULL
for(i in 1:10) preLLn2 <- cor(m3[[i]][LL_vec, "mean"], log(LL))
