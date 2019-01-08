rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
load("~/Dropbox/LES/GL_sim.RData")
library(loo)

P_vec <- paste("p[", 1:n_sample, "]" ,sep = "")
LL_vec <- paste("mu[", 1:n_sample, ",2]" ,sep = "")

# LMA <- list.data.n$LMA
moge <- data_frame(LMA, LMAp, LMAs, LMAp1 = LMA * m1[P_vec, "mean"]) %>%
  mutate(LMAs1 = LMA - LMAp1) %>%
  mutate(LMAp2 = LMA * m2[P_vec, "mean"]) %>%
  mutate(LMAs2 = LMA - LMAp2) %>%
  mutate(LMAp3 = LMA * m3[P_vec, "mean"]) %>%
  mutate(LMAs3 = LMA - LMAp3) %>%
  mutate(preLL1 = exp(m1[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL2 = exp(m2[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL3 = exp(m3[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"]))


moge2 <- data_frame(LMA = rep(moge$LMA, 4),
    p_true =  rep(f, 4),
    LMAp = c(moge$LMAp, moge$LMAp1, moge$LMAp2, moge$LMAp3),
    LMAs = c(moge$LMAs, moge$LMAs1, moge$LMAs2, moge$LMAs3),
    AR = rep(A+R, 4),
    A = rep(A, 4),
    LL = rep(LL, 4),
    model = rep(c("True", "U", "z", "H"), each = n_sample)) %>%
    mutate(model = factor(model,
      levels = c("True", "U", "z", "H"))) %>%
    mutate(p = LMAp / LMA)

GL <- moge2 %>% mutate(dat = "Simulated GL1")

p_est <- extract(res1, pars = P_vec)
obs_cor_A <- NULL
obs_cor_LL <- NULL

true_cor_A <- cor(log(LMAp), log(A))
true_cor_LL <- cor(log(LMAs), log(LL))

before <- proc.time()
for (i in 1:1000){
  temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
  LMAp_temp <- temp * LMA
  LMAs_temp <- LMA - LMAp_temp
  obs_cor_A[i] <- cor(log(LMAp_temp), log(A))
  obs_cor_LL[i] <- cor(log(LMAs_temp), log(LL))
}
after <- proc.time()
after - before

est_cor_A <- mean(obs_cor_A)
est_cor_LL <- mean(obs_cor_LL)

#null GL ================================================================
load("~/Dropbox/LES/GL_sim_null.RData")

cor_A_m <- NULL
cor_LL_m <- NULL
before <- proc.time()

p_est <- list()
for (i in 1:10){
  cor_A <- NULL
  cor_LL <- NULL
  p_est <- extract(res1[[i]], pars = P_vec)
  for (j in 1:1000){
    temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
    LMAp_temp <- temp * LMA
    LMAs_temp <- LMA - LMAp_temp
    cor_A[j] <- cor(log(LMAp_temp), log(A))
    cor_LL[j] <- cor(log(LMAs_temp), log(LL))
  }
  cor_A_m <- cbind(cor_A_m, cor_A)
  cor_LL_m <- cbind(cor_LL_m, cor_LL)
}

after <- proc.time()
after - before

moge <- data_frame(cor_A = c(obs_cor_A, as.vector(cor_A_m)),
      model = rep(c("obs", paste("null", 1:10)), each = 1000),
      model2 = c(rep("obs", 1000), rep("null", 10000)))

p <- ggplot(moge[1:10000,], aes(x = cor_A, fill = model2))
p <- p + geom_histogram(aes(y=0.1*..density..), alpha = 0.8) + theme_bw() +
  geom_vline(xintercept = true_cor_A, linetype = 1) +
  geom_vline(xintercept = est_cor_A, linetype = 2) +
  geom_vline(xintercept = mean(as.vector(cor_A_m)), linetype = 2, color = "gray")
p



#------

load("/Users/mattocci/Dropbox/LES/PA_sim.RData")
moge <- data_frame(LMA, LMAp, LMAs, LMAp1 = LMA * m3[P_vec, "mean"]) %>%
  mutate(LMAs1 = LMA - LMAp1) %>%
  mutate(LMAp2 = LMA * m2[P_vec, "mean"]) %>%
  mutate(LMAs2 = LMA - LMAp2) %>%
  mutate(LMAp3 = LMA * m9[P_vec, "mean"]) %>%
  mutate(LMAs3 = LMA - LMAp3) %>%
  mutate(preLL1 = exp(m3[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL2 = exp(m2[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL3 = exp(m9[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"]))


moge2 <- data_frame(LMA = rep(moge$LMA, 4),
    p_true =  rep(f, 4),
    LMAp = c(moge$LMAp, moge$LMAp1, moge$LMAp2, moge$LMAp3),
    LMAs = c(moge$LMAs, moge$LMAs1, moge$LMAs2, moge$LMAs3),
    AR = rep(A+R, 4),
    A = rep(A, 4),
    LL = rep(LL, 4),
    model = rep(c("True", "U", "z", "H"), each = n_sample)) %>%
    mutate(model = factor(model,
      levels = c("True", "U", "z", "H"))) %>%
    mutate(p = LMAp / LMA)

PA <- moge2 %>% mutate(dat = "Simulated PA")
#######-----------------------------------------------------------------

load("/Users/mattocci/Dropbox/LES/null_sim.RData")
moge <- data_frame(LMA, LMAp, LMAs, LMAp1 = LMA * m1[P_vec, "mean"]) %>%
  mutate(LMAs1 = LMA - LMAp1) %>%
  mutate(LMAp2 = LMA * m2[P_vec, "mean"]) %>%
  mutate(LMAs2 = LMA - LMAp2) %>%
  mutate(LMAp3 = LMA * m3[P_vec, "mean"]) %>%
  mutate(LMAs3 = LMA - LMAp3) %>%
  mutate(preLL1 = exp(m1[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL2 = exp(m2[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL3 = exp(m3[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"]))


moge2 <- data_frame(LMA = rep(moge$LMA, 4),
    p_true =  rep(f, 4),
    LMAp = c(moge$LMAp, moge$LMAp1, moge$LMAp2, moge$LMAp3),
    LMAs = c(moge$LMAs, moge$LMAs1, moge$LMAs2, moge$LMAs3),
    AR = rep(A+R, 4),
    A = rep(A, 4),
    LL = rep(LL, 4),
    model = rep(c("True", "U", "z", "H"), each = n_sample)) %>%
    mutate(model = factor(model,
      levels = c("True", "U", "z", "H"))) %>%
    mutate(p = LMAp / LMA)

moge3 <- moge2 %>% mutate(dat = "Simulated WC") #%>% filter(model != "U")

#------------------------------------------------------------
fig_dat <- bind_rows(GL, PA, moge3) %>%
  filter(model != "z") %>%
  filter(model != "True") %>%
  mutate(model2 = ifelse(model == "U", "Non-hierachical", "Hierachical")) %>%
  mutate(model2 = as.factor(model2))


# p <- ggplot(fig_dat, aes(x = p))
# p <- p + geom_histogram(bins = 10) + facet_grid(model ~ dat)
# p <- p + theme_bw()
# p

# ##
# mf_labeller <- function(p, value){
#   value <- as.character(value)
#   if (var=="model") {
#     value[value=="U"] <- "120 <= alpha~phantom() <= 150"
#     value[value=="H"]  <- "180 <= alpha~phantom() <= 250"
#     value <- lapply(value, function(x) parse(text=x))
#   }
#   return(value)
# }


# levels(fig_dat$model2) <- list("f ~ U[0, 1]", "logit(f) ~ N(mu~phantom(),  sigma~phantom())")
pdf("~/Dropbox/MS/LES_MS/fig/f_sim.PDF", width=5.8, height=3.8)
p <- ggplot(fig_dat, aes(x = p_true, y = p))
p <- p + geom_point() + facet_grid(model2 ~ dat)
p <- p + theme_bw() +  geom_abline(slope=1, lty = 2)
p <- p + xlab("True value")  + ylab("Posterior mean value")
p
dev.off()
