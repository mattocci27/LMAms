rm(list = ls()) # This clears everything from memory.
setwd("~/Dropbox/LES")
library(dplyr)
library(loo)
library(coda)

load("PA_model2.RData")
res2 <- res
summary2 <- fit.summary


load("PA_model2c.RData")
res2c <- res
summary2c <- fit.summary


load("PA_model_alpha.RData")
res_alpha <- res
summary_alpha <- fit.summary


load("PA_model2n.RData")
res2n <- res
summary2n <- fit.summary

load("PA_model2_1.RData")
res21 <- res
summary21 <- fit.summary

load("PA_model2_2.RData")
res22 <- res
summary22 <- fit.summary


load("PA_model2_2_sun.RData")
res22s <- res
summary22s <- fit.summary

load("PA_model2_4.RData")
res24 <- res
summary24 <- fit.summary

load("PA_model_comp.RData")
res_comp <- res
summary_comp <- fit.summary



load("PA_model2_4_2.RData")
res242 <- res
summary242 <- fit.summary


load("PA_model3.RData")
res3 <- res
summary3 <- fit.summary


load("PA_model2main.RData")
res2m <- res
summary2m <- fit.summary
# load("PA_model2_4_3.RData")
# res243 <- res
# summary243 <- fit.summary

load("PA_model2cf_weak.RData")
res2cf_weak <- res
summary2cf_weak <- fit.summary

load("PA_model2cf_non.RData")
res2cf_non <- res
summary2cf_non <- fit.summary


ninf <- extract(res2cf_non, pars = c("log_alpha", "L_sigma"))
winf <- extract(res2cf_weak, pars = c("log_alpha", "L_sigma"))

par(mfrow=c(2,2))
hist(ninf$log_alpha, xlim=c(-1.5, -1.1), breaks=100)
hist(ninf$L_sigma, xlim=c(0,1.2))

hist(winf$log_alpha, xlim=c(-1.5, -1.1),breaks=100)
hist(winf$L_sigma, xlim=c(0,1.2))


N <- 9997666

for (i in 1:500) {
  if (i %/% 0)
  print(i)
}



waic(extract_log_lik(res_alpha,"log_lik"))$waic
waic(extract_log_lik(res_comp,"log_lik"))$waic

waic(extract_log_lik(res2cf_non,"log_lik"))$waic
waic(extract_log_lik(res2cf_weak,"log_lik"))$waic

waic(extract_log_lik(res2m,"log_lik"))$waic

waic(extract_log_lik(res2c,"log_lik"))$waic
waic(extract_log_lik(res2,"log_lik"))$waic

waic(extract_log_lik(res2n,"log_lik"))$waic
waic(extract_log_lik(res21,"log_lik"))$waic
waic(extract_log_lik(res22,"log_lik"))$waic
waic(extract_log_lik(res22s,"log_lik"))$waic
waic(extract_log_lik(res24,"log_lik"))$waic
waic(extract_log_lik(res242,"log_lik"))$waic

waic(extract_log_lik(res3,"log_lik"))$waic

PA <- data

S_vec <- paste("L_Sigma[", rep(1:3, each = 3),"," ,rep(1:3,3), "]" ,sep = "")



summary2cf_non[S_vec,]

P_vec <- paste("p[", 1:nrow(PA), "]" ,sep = "")
Mu_vec <- paste("mu[", 1:nrow(PA), ",2]", sep = "")

Mu_vec1 <- paste("mu[", 1:nrow(PA), ",1]", sep = "")
Mu_vec3 <- paste("mu[", 1:nrow(PA), ",3]", sep = "")

moge1 <- summary2cf_weak[Mu_vec1, "mean"]
moge2 <- summary2cf_weak[Mu_vec, "mean"]
moge3 <- summary2cf_weak[Mu_vec3, "mean"]



PA <- PA %>%
  mutate(p =  summary_comp[P_vec, "mean"]) %>%
  mutate(LMAp =  summary_comp[P_vec, "mean"] * LMA) %>%
  mutate(LMAs = LMA - LMAp) %>%
  mutate(preLL = exp(summary_comp[Mu_vec, "mean"])) %>%
  mutate(p_c =  summary2cf_non[P_vec, "mean"]) %>%
  mutate(LMAp_c =  summary2cf_non[P_vec, "mean"] * LMA) %>%
  mutate(LMAs_c = LMA - LMAp_c) %>%
  mutate(preLL_c = exp(summary2cf_non[Mu_vec, "mean"]))

PA <- PA %>%
  mutate(p =  summary2cf_weak[P_vec, "mean"]) %>%
  mutate(LMAp =  summary2cf_weak[P_vec, "mean"] * LMA) %>%
  mutate(LMAs = LMA - LMAp) %>%
  mutate(preLL = exp(summary2cf_weak[Mu_vec, "mean"])) %>%
  mutate(p_c =  summary2cf_non[P_vec, "mean"]) %>%
  mutate(LMAp_c =  summary2cf_non[P_vec, "mean"] * LMA) %>%
  mutate(LMAs_c = LMA - LMAp_c) %>%
  mutate(preLL_c = exp(summary2cf_non[Mu_vec, "mean"]))

plot(preLL ~ LL, log = "xy", data = PA)

plot(Rarea ~ LMAp, log = "xy", data = PA)

cor.test(log(PA$LL), log(PA$preLL))
cor.test(log(PA$Aarea), log(PA$LMAp))
cor.test(log(PA$Rarea), log(PA$LMAp))


par(mfrow = c(1,3))
plot(Aarea ~ LMAp, PA, log = "xy")
plot(Rarea ~ LMAp, PA, log = "xy")
plot(LL ~ preLL, PA, log = "xy")


par(mfrow = c(1,2))
plot(LMAp_c ~ LMAp, PA)
plot(LMAs_c ~ LMAs, PA)



cor.test(log(PA$LL), log(PA$preLL_c))
cor.test(log(PA$Aarea), log(PA$LMAp_c))
cor.test(log(PA$Rarea), log(PA$LMAp_c))

var(sun$Aarea)
var(shade$Aarea)
var(sun$Rarea)
var(shade$Rarea)

library(shinystan)
moge <- launch_shinystan(res2c)

sun <- data[data$strata=="CAN",]
shade <- data[data$strata=="UNDER",]

max(sun$Aarea)/min(sun$Aarea)
max(shade$Aarea)/min(shade$Aarea)

min(sun$Aarea/sun$Rarea)
min(shade$Aarea/shade$Rarea)

max(sun$Rarea)/min(sun$Rarea)
max(shade$Rarea)/min(shade$Rarea)

summary(sun$Aarea/sun$Rarea) # 2.578


plot(p ~p_c, data =PA)
min(sun$Aarea - sun$Rarea * 300)


min(sun$Aarea - sun$Rarea )
min(0.25 * shade$Aarea - shade$Rarea)



par(mfrow=c(2,2))
hist(rnorm(1000, 0, sqrt(10)))
hist(rnorm(1000, 0, sqrt(100)))
hist(rnorm(1000, 0, sqrt(1000)))
hist(rnorm(1000, 0, sqrt(10000)))
par(mfrow=c(1,1))


X <- matrix(c(1,-0.8,-0.8,1),ncol=2)

A <- chol(X)


t(A) %*% A
